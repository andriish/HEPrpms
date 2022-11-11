#include "f90cache.h"

/* the base cache directory */
char *cache_dir = NULL;
char cache_dir_allocated = 0;

/* the directory for temporary files */
static char *temp_dir = NULL;

/* the debug logfile name, if set */
char *cache_logfile = NULL;

/* the argument list after processing */
static ARGS *stripped_args;

/* the original argument list */
static ARGS *orig_args;

/* the dependance module list */
static ARGS *depmod_list;

/* the output object file being compiled to */
static char *output_obj;

/* the output precompiled (sub)module file being compiled to */
static char *output_dir_mod;

/* the source file */
static char *input_file;

/* the name of the file containing the cached object code */
static char *hashname;

/* the extension of the file after pre-processing */
static const char *i_extension;

/* the name of the temporary pre-processed file */
static char *i_tmpfile;

/* are we compiling a .f or .f90 file directly? */
static int direct_i_file;

/* are we making a .mod file only? */
static int mod_file_only = 0;

/* the name of the cpp stderr file */
static char *fpp_stderr;

/* the name of the statistics file */
char *stats_file = NULL;

/* a list of supported file extensions, and the equivalent
   extension for code that has been through the pre-processor */
static struct {
    char *extension;
    char *i_extension;
} extensions[] = {
    {"F", "f"},
    {"f", "f"},
    {"F90", "f90"},
    {"f90", "f90"},
    {"F95", "f95"},
    {"f95", "f95"},
    {"F03", "f03"},
    {"f03", "f03"},
    {NULL, NULL}
};

/* the f90 compiler type: see f90cache.h */
static int f90_compiler_type = 0;

/* the GNU version */
static char *GNU_VERSION = NULL;

/* the GNU major version (numerical value) */
static int GNU_MAJOR_VERSION_NUM = -1;

/* the GNU minor version (numerical value) */
static int GNU_MINOR_VERSION_NUM = -1;

/* the GNU preprocessor */
static char *GNU_preprocessor = NULL;

/* something went wrong - just execute the real compiler */
static void failed(void)
{
    char *e;

    /* delete intermediate pre-processor file if needed */
    if (i_tmpfile) {
	if (!direct_i_file) {
	    unlink(i_tmpfile);
	}
	free(i_tmpfile);
	i_tmpfile = NULL;
    }

    /* delete the fpp stderr file if necessary */
    if (fpp_stderr) {
	unlink(fpp_stderr);
	free(fpp_stderr);
	fpp_stderr = NULL;
    }

    /* strip any special cache args */
    args_strip(orig_args, "--f90cache-skip");
    args_strip_2(orig_args, "--f90cache-depmod");

    if ((e=getenv("F90CACHE_PREFIX"))) {
	char *p = find_executable(e, MYNAME);
	if (!p) {
	    perror(e);
	    exit(1);
	}
	args_add_prefix(orig_args, p);
    }

    execv(orig_args->argv[0], orig_args->argv);
    fc_log("'execv' returned (%s)\n", strerror(errno));
    perror(orig_args->argv[0]);
    exit(1);
}

/* return a string to be used to distinguish temporary files
   this also tries to cope with NFS by adding the local hostname
*/
static const char *tmp_string(void)
{
    static char *ret;

    if (!ret) {
	char hostname[200];
	strcpy(hostname, "unknown");
#if HAVE_GETHOSTNAME
	gethostname(hostname, sizeof(hostname)-1);
#endif
	hostname[sizeof(hostname)-1] = 0;
	if (asprintf(&ret, "%s.%u", hostname, (unsigned)getpid()) == -1) {
	    failed();
	}
    }

    return ret;
}

/* run the real compiler and put the result in cache
     tmp_stdout   : result of STDOUT (should be empty)
     tmp_stderr   : result of STDERR
     tmp_hashname : result of the compiler (OBJECT FILE)
 */
static void to_cache(ARGS *args)
{
    char *path_stderr;
    char *tmp_stdout, *tmp_stderr, *tmp_hashname;
    struct stat st1, st2, st3;
    int status;
    char *mod_dir, *mod_to, *hash_dir, *mod_include;
    char *mod_file;
    DIR *dir_fd;
    struct dirent *dir_entry;
    int mod_num;

    x_asprintf(&tmp_stdout, "%s/tmp.stdout.%s", temp_dir, tmp_string());
    x_asprintf(&tmp_stderr, "%s/tmp.stderr.%s", temp_dir, tmp_string());

    if (!mod_file_only) {
	x_asprintf(&tmp_hashname, "%s/tmp.hash.%s.o", temp_dir, tmp_string());
	args_add(args, "-o");
	args_add(args, tmp_hashname);
    }

    /* created (sub)modules must go into a specific module directory;
       waiting the hash name, the module dir name is based on the PID */
    x_asprintf(&mod_dir, "%s/%u", temp_dir, (unsigned)getpid());
    create_dir(mod_dir);

    if (getenv("F90CACHE_FPP2")) {
	args_add(args, input_file);
    } else {
	args_add(args, i_tmpfile);
    }

    /* add a flag to specify where module files are to be put;
       -> the flag work also for submodule files */
    switch (f90_compiler_type) {
	case GNU_GFC:
	    /* -J flag works with 4.3, but it is an alias of -M
		which is not supported here. Therefore, we require
		that gfortran is at least 4.4 */
	    x_asprintf(&mod_to, "-J%s", mod_dir);
	    args_add(args, mod_to);
	    free(mod_to);
	    break;
	case INTEL_IFC:
	    /* warning: for the INTEL ifort compiler, don't put the
	       two 'blank separated' strings in only one arg */
	    args_add(args, "-module");
	    args_add(args, mod_dir);
	    break;
	default:
	    printf("%s: Fortran 90 compiler not supported\n",MYNAME);
	    fc_log("Unsupported compiler\n");
	    fatal("to_cache");
	    break;
    }

    /* if there was a module directory specified in the compile flags, we
       have to add that directory to the include path to ensure module files
       can be found. */
    if (output_dir_mod) {
        x_asprintf(&mod_include, "-I%s", output_dir_mod);
        args_add(args, mod_include);
        free(mod_include);
    }

    /* we can remove all preprocessing flags */
    args_strip(args,"-D");
    status = execute(args->argv, tmp_stdout, tmp_stderr);

    if (!mod_file_only) {
	args_pop(args, 4); /* remove the last args */
	if (f90_compiler_type==INTEL_IFC) {
	    args_pop(args, 1); /* remove one more */
	}
    } else {
	args_pop(args, 2); /* remove the last args */
    }

    /* normally, there is no stdout */
    if (stat(tmp_stdout, &st1) != 0 || st1.st_size != 0) {
	fc_log("Compiler produced stdout for '%s'\n", output_obj);
	stats_update(STATS_STDOUT);
	unlink(tmp_stdout);
	unlink(tmp_stderr);
	unlink(tmp_hashname);
	failed();
    }
    unlink(tmp_stdout);

    /* compilation failed */
    if (status != 0) {
	int fd;
	fc_log("Compilation of '%s' gave status: %d\n", input_file, status);
	stats_update(STATS_STATUS);

	rmdir(mod_dir);
	free(mod_dir);

	fd = open(tmp_stderr, O_RDONLY | O_BINARY);
	if (fd != -1) {
	    if (mod_file_only || strcmp(output_obj, "/dev/null") == 0 ||
		 rename(tmp_hashname, output_obj) == 0 || errno == ENOENT) {
		if (fpp_stderr) {
		    /* we might have some stderr from fpp */
		    int fd2 = open(fpp_stderr, O_RDONLY | O_BINARY);
		    if (fd2 != -1) {
			copy_fd(fd2, 2);
			close(fd2);
			unlink(fpp_stderr);
			fpp_stderr = NULL;
		    }
		}

		/* we can use a quick method of
		   getting the failed output */
		copy_fd(fd, 2);
		close(fd);
		unlink(tmp_stderr);
		if (i_tmpfile && !direct_i_file) {
		    unlink(i_tmpfile);
		}
		exit(status);
	    }
	}

	unlink(tmp_stderr);
	unlink(tmp_hashname);
	failed();
    }

    x_asprintf(&path_stderr, "%s.stderr", hashname);

    if (!mod_file_only && stat(tmp_hashname, &st2) == 0) {
	if (rename(tmp_hashname, hashname) != 0) {
	    fc_log("Failed to rename tmp hashname files '%s'\n", strerror(errno));
	    stats_update(STATS_ERROR);
	    failed();
	}
    }
    if (stat(tmp_stderr, &st1) != 0 ||
	rename(tmp_stderr, path_stderr) != 0) {
	fc_log("Failed to rename tmp stderr files '%s'\n", strerror(errno));
	stats_update(STATS_ERROR);
	failed();
    }

    /* count files in mod_dir */
    dir_fd = opendir(mod_dir);
    if (!dir_fd) {
	fc_log("Failed to read module directory '%s'\n",mod_dir);
	failed();
    }
    mod_num = 0;
    while ((dir_entry = readdir(dir_fd))) {
	if (strcmp(dir_entry->d_name,".") == 0) continue;
	if (strcmp(dir_entry->d_name,"..") == 0) continue;
	mod_num++;
	x_asprintf(&mod_file, "%s/%s", mod_dir, dir_entry->d_name);
	if (stat(mod_file, &st3) == 0) {
	    stats_tocache(file_size(&st3));
	} else {
	    fc_log("Failed to stat the (sub)module file '%s'\n",dir_entry->d_name);
	}
	free(mod_file);
    }
    if (mod_num > 0) {
	char *command;
	/* creating the cache module directory with the '.dir' extension */
	x_asprintf(&hash_dir, "%s.dir", hashname);
	if (create_dir(hash_dir) != 0) {
	    fc_log("Failed to create '%s'\n",hash_dir);
	    failed();
	}
	/* move all (sub)modules from mod_dir to hash_dir */
/* removing the following statement, else when there is no .mod file,
   the system complains with:
   mv: cannot stat ‘~/.f90cache/13621/ *.mod’: No such file or directory
   (even we add the "-f" flag) */
//	x_asprintf(&command,"mv %s/*.mod %s/*.smod %s",mod_dir,mod_dir,hash_dir);
	x_asprintf(&command,"mv %s/* %s",mod_dir,hash_dir);
	free(hash_dir);
	if (system(command) == -1) {
	    failed();
	}
	free(command);
	if (mod_num==1) {
	    fc_log("Placed one precompiled Fortran (sub)module into cache\n");
	} else {
	    fc_log("Placed %i precompiled Fortran (sub)modules into cache\n", mod_num);
	}
    }
    rmdir(mod_dir);
    free(mod_dir);

    if (output_obj) {
	fc_log("Placed the object file '%s' into cache\n", output_obj);
	/* keep two calls and not only one */
	stats_tocache(file_size(&st1));
	stats_tocache(file_size(&st2));
    } else {
	stats_tocache(file_size(&st1));
    }

    if (!mod_file_only) {
	free(tmp_hashname);
    }
    free(tmp_stderr);
    free(tmp_stdout);
    free(path_stderr);
}

/* find the hash for a command. The hash includes most of arguments,
   plus the output from running the compiler with -E */
static void find_hash( ARGS *args )
{
    int i;
    char *path_stdout, *path_stderr;
    char *hash_dir;
    char *s;
    struct stat st;
    int status;
    int nlevels = 2;
    char *input_base;

    if ((s = getenv("F90CACHE_NLEVELS"))) {
	nlevels = atoi(s);
	if (nlevels < 1) nlevels = 1;
	if (nlevels > 8) nlevels = 8;
    }

    hash_start();

    /* we have to hash the extension (after pre-processing),
       as a .f file (fixed format) isn't treated the same by the
       compiler as a .f90 file (free format) */
    hash_string(i_extension);

    /* first the arguments */
    for (i=1;i<args->argc;i++) {
	/* some arguments don't contribute to the hash. The
	   theory is that these arguments will change the
	   output of -E if they are going to have any effect
	   at all, or they only affect linking */
	if (i < args->argc-1) {
	    if (strcmp(args->argv[i], "-I") == 0 ||
		strcmp(args->argv[i], "-L") == 0 ||
		strcmp(args->argv[i], "-D") == 0) {
		i++; /* skip also next arg */
		continue;
	    }
	}

	if (f90_compiler_type==GNU_GFC) {
	    if (i < args->argc-1) {
		if (strcmp(args->argv[i], "-include") == 0 ||
		    strcmp(args->argv[i], "-idirafter") == 0 ||
		    strcmp(args->argv[i], "-isystem") == 0) {
		    i++; /* skip also next arg */
		    continue;
		}
	    }
	}

	if (strncmp(args->argv[i], "-I", 2) == 0 ||
	    strncmp(args->argv[i], "-L", 2) == 0 ||
	    strncmp(args->argv[i], "-D", 2) == 0 ||
	    strcmp(args->argv[i], "-fsyntax-only") == 0) {
	    continue;
	}

	if (f90_compiler_type==GNU_GFC) {
	    if (strncmp(args->argv[i], "-idirafter", 10) == 0 ||
		strncmp(args->argv[i], "-isystem", 8) == 0) {
		continue;
	    }
	}

	if (f90_compiler_type==INTEL_IFC) {
	    if (strcmp(args->argv[i], "-y") == 0) {
		continue;
	    }
	}

	if (strncmp(args->argv[i], "--specs=", 8) == 0 &&
	     stat(args->argv[i]+8, &st) == 0) {
	    /* if given a explicit specs file, then hash that file, but
	       don't include the path to it in the hash */
	    hash_file(args->argv[i]+8);
	    continue;
	}

	/* all other arguments are included in the hash */
	hash_string(args->argv[i]);
    }

    /* the compiler driver size and date. This is a simple minded way
       to try and detect compiler upgrades. It is not 100% reliable */
    if (stat(args->argv[0], &st) != 0) {
	fc_log("Couldn't stat the compiler! (argv[0]='%s')\n", args->argv[0]);
	stats_update(STATS_COMPILER);
	failed();
    }
    hash_int(st.st_size);
    hash_int(st.st_mtime);

    /* also include the hash of the depending (sub)module files -
       list has been stored in depmod_list. */
    if (depmod_list) {
	for (i=0;i<depmod_list->argc;i++) {
	    hash_module_file( depmod_list->argv[i], f90_compiler_type,
			      GNU_MAJOR_VERSION_NUM, GNU_MINOR_VERSION_NUM );
	}
    }

    /* also include the hash of the compiler name - as some compilers
       use hard links and behave differently depending on the real name */
    if (st.st_nlink > 1) {
	hash_string(str_basename(args->argv[0]));
    }

    /* possibly hash the current working directory */
    if (getenv("F90CACHE_HASHDIR")) {
	char *cwd = gnu_getcwd();
	if (cwd) {
	    hash_string(cwd);
	    free(cwd);
	}
    }

    /* ~/hello.F -> tmp.hello.hostname.f */
    input_base = str_basename(input_file);

    /* now the run */
    x_asprintf(&path_stdout, "%s/%s.tmp.%s.%s", temp_dir,
		input_base, tmp_string(), i_extension);
    free(input_base);
    x_asprintf(&path_stderr, "%s/tmp.fpp_stderr.%s", temp_dir,
		tmp_string());

    if (!direct_i_file) {
	/* run preprocessing on the input file to obtain the .f, .f90
	   or .f95 file */
	int i_c;

	args_add(args, "-E");

	/* change "-c" option to "-w" in order to avoid a warning
	   on some compilers */
	i_c = -1;
	for (i=0;i<args->argc;i++) {
	    if (strcmp(args->argv[i], "-c") == 0) {
		args->argv[i][1] = 'w'; /* blank or empty string
					   doesn't work ! */
		i_c = i;
		break;
	    }
	}

	args_add(args, input_file);

	status = execute(args->argv, path_stdout, path_stderr);

	if (f90_compiler_type==GNU_GFC) {
	    /* an empty *.s must be deleted in the working directory
	      (bug when preprocessing with 'gfortran -E', for 4.4 and 4.5) */
	    int i, i1, i2;
	    char asmfile[200];
	    int lenfi = strlen(input_file);
	    int len, found = 0;
	    for (i=lenfi-1;i>0;i--) {
		if ( input_file[i] == '/' ) {
		    found = 1;
		    break;
		}
	    }
	    if( found == 1 ) {
		i1 = i + 1;
	    } else {
		i1 = 0;
	    }
	    len = lenfi - i1;
	    strncpy(asmfile, &input_file[i1], len);
	    asmfile[len] = '\0';
	    for (i=strlen(asmfile)-1;i>0;i--) {
		if ( asmfile[i] == '.' ) {
		    break;
		}
	    }
	    i2 = i + 1;
	    asmfile[i2] = 's';
	    asmfile[i2+1] = '\0';
	    unlink(asmfile);
	}

	/* if applicable, restore "-c" option for later execution */
	if (i_c >= 0) {
	    args->argv[i_c][1] = 'c';
	}

	/* removing "-E" and input_file */
	args_pop(args, 2);


    } else {
	/* we are compiling a .f, .f90 or .f95 file - that means we can
	   skip the fpp stage and directly form the correct i_tmpfile */
	free(path_stdout);
	path_stdout = input_file;
	if (create_empty_file(path_stderr) != 0) {
	    stats_update(STATS_ERROR);
	    fc_log("Failed to create empty stderr file\n");
	    failed();
	}
	status = 0;
    }

    if (status != 0) {
	if (!direct_i_file) {
	    unlink(path_stdout);
	}
	unlink(path_stderr);
	fc_log("The preprocessor gave status: %d\n", status);
	stats_update(STATS_PREPROCESSOR);
	failed();
    }

    hash_file(path_stdout);
    hash_file(path_stderr);

    if (direct_i_file) {
	if (asprintf(&i_tmpfile, "%s", path_stdout) == -1) {
	    failed();
	}
    } else {
	i_tmpfile = path_stdout;
    }

    if (!getenv("F90CACHE_FPP2")) {
	/* if we are using the FPP trick then we need to remember this
	   stderr data and output it just before the main stderr from
	   the compiler pass */
	fpp_stderr = path_stderr;
    } else {
	unlink(path_stderr);
	free(path_stderr);
    }

    /* we use a N level subdir for the cache path to reduce the impact
       on filesystems which are slow for large directories */
    s = hash_result();
    x_asprintf(&hash_dir, "%s/%c", cache_dir, s[0]);
    x_asprintf(&stats_file, "%s/stats", hash_dir);
    for (i=1; i<nlevels; i++) {
	char *p;
	if (create_dir(hash_dir) != 0) {
	    fc_log("Failed to create %s\n", hash_dir);
	    failed();
	}
	x_asprintf(&p, "%s/%c", hash_dir, s[i]);
	free(hash_dir);
	hash_dir = p;
    }
    if (create_dir(hash_dir) != 0) {
	fc_log("Failed to create '%s'\n", hash_dir);
	failed();
    }
    x_asprintf(&hashname, "%s/%s", hash_dir, s+nlevels);
    free(hash_dir);
}

/*
   try to return the compile result from cache. If we can return from
   cache then this function exits with the correct status code,
   otherwise it returns */
static void from_cache(int first)
{
    int fd_stderr, fd_fpp_stderr;
    char *stderr_file;
    struct stat st;
    char *hash_dir, *command;

    x_asprintf(&stderr_file, "%s.stderr", hashname);
    fd_stderr = open(stderr_file, O_RDONLY | O_BINARY);
    if (fd_stderr == -1) {
	/* it isn't in cache ... */
	free(stderr_file);
	return;
    }

    /* make sure the output is there too */
    if (!mod_file_only && stat(hashname, &st) != 0) {
	close(fd_stderr);
	unlink(stderr_file);
	free(stderr_file);
	return;
    }

    /* the user might be disabling cache hits */
    if (first && getenv("F90CACHE_RECACHE")) {
	close(fd_stderr);
	unlink(stderr_file);
	free(stderr_file);
	return;
    }

    /* copy the (sub)module(s), if they exist, in the target module directory */
    DIR *dir_fd;
    x_asprintf(&hash_dir, "%s.dir", hashname);
    dir_fd = opendir(hash_dir);
    if (dir_fd) {
	if (output_dir_mod) {
/* see remark on line 317 */
//	    x_asprintf(&command,"cp %s/*.mod %s/*.smod '%s'",hash_dir,hash_dir,output_dir_mod);
	    x_asprintf(&command,"cp %s/* '%s'",hash_dir,output_dir_mod);
	    free(hash_dir);
	    if (system(command) != 0) {
		fc_log("Failed to copy (sub)modules to %s\n",output_dir_mod);
		failed();
	    }
	} else {
	    char *cwd = gnu_getcwd();
/* see remark on line 317 */
//	    x_asprintf(&command,"cp %s/*.mod %s/*.smod '%s'",hash_dir,hash_dir,cwd);
	    x_asprintf(&command,"cp %s/* '%s'",hash_dir,cwd);
	    free(hash_dir);
	    if (system(command) != 0) {
		fc_log("Failed to copy (sub)modules to '%s'\n",cwd);
		failed();
	    }
	    free(cwd);
	}
	free(command);
	if (first) {
	    fc_log("Got cached some (sub)module(s)\n");
	    stats_update(STATS_CACHED);
	}
    } else {
	free(hash_dir);
    }
    free(dir_fd);

    utime(stderr_file, NULL);
    /* why not do the same time update to hashname ?
       in this way, we could always use F90CACHE_HARDLINK,
       even with Makefiles */

    if (!mod_file_only) {
	int ret;
	if (strcmp(output_obj, "/dev/null") == 0) {
	    ret = 0;
	} else {
	    unlink(output_obj);
	    if (getenv("F90CACHE_HARDLINK")) {
		ret = link(hashname, output_obj);
	    } else {
		ret = copy_file(hashname, output_obj);
	    }
	}

	/* the hash file might have been deleted by some external process */
	if (ret == -1 && errno == ENOENT) {
	    fc_log("Hash file missing for %s\n", output_obj);
	    stats_update(STATS_MISSING);
	    close(fd_stderr);
	    unlink(stderr_file);
	    return;
	}
	free(stderr_file);

	if (ret == -1) {
	    ret = copy_file(hashname, output_obj);
	    if (ret == -1) {
		fc_log("Failed to copy '%s' -> '%s' (%s)\n",
			hashname, output_obj, strerror(errno));
		stats_update(STATS_ERROR);
		failed();
	    }
	}
	/* update the mtime on the file so that make doesn't get confused */
	utime(output_obj, NULL);
    }

    /* get rid of the intermediate preprocessor file */
    if (i_tmpfile) {
	if (!direct_i_file) {
	    unlink(i_tmpfile);
	}
	free(i_tmpfile);
	i_tmpfile = NULL;
    }

    /* send the fpp stderr, if applicable */
    fd_fpp_stderr = open(fpp_stderr, O_RDONLY | O_BINARY);
    if (fd_fpp_stderr != -1) {
	copy_fd(fd_fpp_stderr, 2);
	close(fd_fpp_stderr);
	unlink(fpp_stderr);
	free(fpp_stderr);
	fpp_stderr = NULL;
    }

    /* send the stderr */
    copy_fd(fd_stderr, 2);
    close(fd_stderr);

    /* and exit with the right status code */
    if (first && !mod_file_only) {
	fc_log("Got cached result for '%s'\n", output_obj);
	stats_update(STATS_CACHED);
    }

    args_clean(orig_args);
    args_clean(depmod_list);
    free(hashname);
    free(stats_file);
    if (cache_dir_allocated)
	free(cache_dir);
    exit(0);
}

/* find the real compiler. We just search the PATH to find an executable of the
   same name that isn't a link to ourselves */
static void find_compiler(int argc, char **argv)
{
    char *base;
    char *path;

    orig_args = args_init(argc, argv);

    base = str_basename(argv[0]);

    /* we might be being invoked like "f90cache gfortran -c foo.f" */
    if (strcmp(base, MYNAME) == 0) {
	args_remove_first(orig_args);
	free(base);
	if (strchr(argv[1],'/')) {
	    /* a full path was given */
	    return;
	}
	base = str_basename(argv[1]);
    }

    /* support user override of the compiler */
    if ((path=getenv("F90CACHE_FC"))) {
	base = strdup(path);
	free(path);
    }

    orig_args->argv[0] = find_executable(base, MYNAME);

    /* can't find the compiler! */
    if (!orig_args->argv[0]) {
	stats_update(STATS_COMPILER);
	perror(base);
	free(base);
	exit(1);
    }
    free(base);
}

/* check a filename for Fortran extension. Return the pre-processor extension */
static const char *check_extension(const char *fname, int *direct_i)
{
    int i;
    const char *p;

    if (direct_i) {
	*direct_i = 0;
    }

    p = strrchr(fname, '.');
    if (!p) return NULL;
    p++;
    for (i=0; extensions[i].extension; i++) {
	if (strcmp(p, extensions[i].extension) == 0) {
	    if (direct_i && strcmp(p, extensions[i].i_extension) == 0) {
		*direct_i = 1;
	    }
	    p = getenv("F90CACHE_EXTENSION");
	    if (p) return p;
	    return extensions[i].i_extension;
	}
    }
    return NULL;
}

/*
   process the compiler options to form the correct set of options
   for obtaining the preprocessor output
*/
static void process_args(int argc, char **argv)
{
    int i;
    int found_c_opt = 0;
    struct stat st;
    char *e;

    stripped_args = args_init(0, NULL);
    depmod_list = args_init(0, NULL);

    args_add(stripped_args, argv[0]);

    for (i=1; i<argc; i++) {
	/* some options will never work ... */
	if (strcmp(argv[i], "-E") == 0) {
	    fc_log("Option %s leads to ignore caching\n", argv[i]);
	    failed();
	}

	/* these are too hard */
	if (strcmp(argv[i], "-x") == 0) {
	    fc_log("Option %s is unsupported\n", argv[i]);
	    stats_update(STATS_UNSUPPORTED);
	    failed();
	    continue;
	}

	/* we must have "compile only" or "syntax only" */
	if (strcmp(argv[i], "-c") == 0) {
	    args_add(stripped_args, argv[i]);
	    found_c_opt = 1;
	    continue;
	}
	if (strcmp(argv[i], "-fsyntax-only") == 0) {
	    args_add(stripped_args, argv[i]);
	    found_c_opt = 1;
	    mod_file_only = 1;
	    continue;
	}
	if (f90_compiler_type==INTEL_IFC) {
	    if (strcmp(argv[i], "-y") == 0) {
		args_add(stripped_args, argv[i]);
		found_c_opt = 1;
		mod_file_only = 1;
		continue;
	    }
	}

	/* where the output object file must go */
	if (strcmp(argv[i], "-o") == 0) {
	    if (i == argc-1) {
		fc_log("Missing argument to %s\n", argv[i]);
		stats_update(STATS_ARGS);
		failed();
	    }
	    output_obj = argv[++i];
	    continue;
	}

	/* alternate form of -o, with no space;
	   but exclude the '-openmp' flag of INTEL_IFC */
	if (strncmp(argv[i], "-o", 2) == 0) {
	    if (f90_compiler_type != INTEL_IFC ||
		strcmp(argv[i], "-openmp") != 0) {
		output_obj = &argv[i][2];
		continue;
	    }
	}


	/* where the output precompiled (sub)module file must go */
	if (f90_compiler_type==GNU_GFC) {
	    if (strncmp(argv[i], "-J", 2) == 0) {
		output_dir_mod = &argv[i][2];
		continue;
	    }
	} else if (f90_compiler_type==INTEL_IFC) {
	    if (strcmp(argv[i], "-module") == 0) {
		if (i == argc-1) {
		    fc_log("Missing argument to %s\n", argv[i]);
		    stats_update(STATS_ARGS);
		    failed();
		}
		output_dir_mod = argv[++i];
		continue;
	    }
	}

	/* debugging is handled specially, so that we know if we
	   can strip line number info
	*/
	if (strncmp(argv[i], "-g", 2) == 0) {
	    args_add(stripped_args, argv[i]);
	    continue;
	}

	/* the user knows best: just swallow the next arg */
	if (strcmp(argv[i], "--f90cache-skip") == 0) {
	    i++;
	    if (i == argc) {
		fc_log("Missing argument to '--f90cache-skip'\n");
		failed();
	    }
	    args_add(stripped_args, argv[i]);
	    continue;
	}

	/* special f90cache option */
	if (strcmp(argv[i], "--f90cache-depmod") == 0) {
	    i++;
	    if (i == argc) {
		fc_log("Missing argument to '--f90cache-depmod'\n");
		failed();
	    }
	    /* next argument must be a precompiled (sub)module file
	       (i.e. with suffix .mod OR .smod) */
	    if ( (!strcmp(argv[i]+strlen(argv[i])-4,".mod") == 0) &&
		 (!strcmp(argv[i]+strlen(argv[i])-5,".smod") == 0)
	       ) {
		fc_log("Argument following '--f90cache-depmod' must be a precompiled (sub)module file\n");
		failed();
	    }
	    args_add(depmod_list, argv[i]);
	    continue;
	}

	/* options that take an argument */
	if (f90_compiler_type==GNU_GFC) {
	    const char *opts[] = {"-I", "-L", "-D", "-U", "-Xlinker",
				  "-fintrinsic-modules-path",
				  NULL};
	    int j;
	    for (j=0;opts[j];j++) {
		if (strcmp(argv[i], opts[j]) == 0) {
		    if (i == argc-1) {
			fc_log("Missing argument to %s\n",
				 argv[i]);
			stats_update(STATS_ARGS);
			failed();
		    }

		    args_add(stripped_args, argv[i]);
		    args_add(stripped_args, argv[++i]);
		    break;
		}
	    }
	    if (opts[j]) continue;
	} else if (f90_compiler_type==INTEL_IFC) {
	    const char *opts[] = {"-I", "-L", "-D", "-U", "-Xlinker",
				  "-align", "-arch", "-assume",
				  "-ccdefault", "-check", "-convert",
				  "-debug", "-fp-model", "-stand", "-warn",
				  NULL};
	    int j;
	    for (j=0;opts[j];j++) {
		if (strcmp(argv[i], opts[j]) == 0) {
		    if (i == argc-1) {
			fc_log("Missing argument to %s\n",
				 argv[i]);
			stats_update(STATS_ARGS);
			failed();
		    }

		    args_add(stripped_args, argv[i]);
		    args_add(stripped_args, argv[++i]);
		    break;
		}
	    }
	    if (opts[j]) continue;
	}

	/* other options */
	if (argv[i][0] == '-') {
	    args_add(stripped_args, argv[i]);
	    continue;
	}

	/* if an argument isn't a plain file then assume it's an option,
	   not an input file. This allows us to cope better with
	   unusual compiler options */
	if (stat(argv[i], &st) != 0 || !S_ISREG(st.st_mode)) {
	    args_add(stripped_args, argv[i]);
	    continue;
	}

	if (input_file) {
	    if (check_extension(argv[i], NULL)) {
		fc_log("Multiple input files ('%s' and '%s')\n",
			 input_file, argv[i]);
		stats_update(STATS_MULTIPLE);
	    } else if (!found_c_opt) {
		fc_log("Call for link\n");
		stats_update(STATS_LINK);
	    } else {
		fc_log("Not a Fortran file: '%s'\n", argv[i]);
		stats_update(STATS_NOTF);
	    }
	    failed();
	}

	input_file = argv[i];
    }

    /* hereafter, all arguments have been processed */
    if (!input_file) {
	fc_log("No input file\n");
	stats_update(STATS_NOINPUT);
	failed();
    }

    i_extension = check_extension(input_file, &direct_i_file);
    if (i_extension == NULL) {
	if (!found_c_opt) {
	    fc_log("Call for link\n");
	    stats_update(STATS_LINK);
	} else {
	    fc_log("Not a Fortran file: '%s'\n", input_file);
	    stats_update(STATS_NOTF);
	}
	failed();
    }

    if (!found_c_opt) {
	fc_log("Neither -c or -fsyntax-only (or equivalent) option found for '%s'\n", input_file);
	stats_update(STATS_LINK);
	failed();
    }

    /* don't try to second guess the compilers heuristics for stdout
       handling */
    if (output_obj && strcmp(output_obj, "-") == 0) {
	stats_update(STATS_OUTSTDOUT);
	failed();
    }

    if (!output_obj && !mod_file_only) {
	char *p;
	output_obj = x_strdup(input_file);
	if ((p = strrchr(output_obj, '/'))) {
	    output_obj = p+1;
	}
	p = strrchr(output_obj, '.');
	if (!p || !p[1]) {
	    fc_log("Badly formed output_obj '%s'\n", output_obj);
	    stats_update(STATS_ARGS);
	    failed();
	}
	p[1] = 'o';
	p[2] = 0;
    }

    /* cope with -o /dev/null */
    if (output_obj && strcmp(output_obj,"/dev/null") != 0 &&
	stat(output_obj, &st) == 0 && !S_ISREG(st.st_mode)) {
	fc_log("Not a regular file: '%s'\n", output_obj);
	stats_update(STATS_DEVICE);
	failed();
    }

    if ((e=getenv("F90CACHE_PREFIX"))) {
	char *p = find_executable(e, MYNAME);
	if (!p) {
	    perror(e);
	    exit(1);
	}
	args_add_prefix(stripped_args, p);
    }
}

/* the main f90cache driver function */
static void f90cache_driver(int argc, char *argv[])
{
    char *compiler_fullname;
    char *compiler_name;
    char *compiler_path;
    char *command;
    FILE *fp;

    /* to separate each call of f90cache */
    fc_log("\n");

    /* find the real compiler */
    find_compiler(argc, argv);
    compiler_name = str_basename(orig_args->argv[0]);
    if (strncmp(compiler_name, "gfortran", 8) == 0) {
	/* store the dirpath of the compiler */
	compiler_path = dirname(orig_args->argv[0]);
	compiler_fullname = orig_args->argv[0];

	/* find GNU_MAJOR_VERSION_NUM */
	x_asprintf(&command,"%s --version | sed -n 1p | sed -n '/^[^0-9]*\\([0-9]\\).*/ s//\\1/p'",compiler_fullname);
	fp = popen(command, "r");
	if (fp == NULL) {
	    printf("Failed to get GNU_MAJOR_VERSION_NUM\n" );
	    exit(1);
	}
	if (fscanf( fp, "%i", &GNU_MAJOR_VERSION_NUM) == -1) {
	    fatal("Failed to read GNU_MAJOR_VERSION_NUM from shell command");
	}
	pclose(fp);
	/* GNU major version must be 4 to 9 */
	if (GNU_MAJOR_VERSION_NUM<4 || 9<GNU_MAJOR_VERSION_NUM) {
	    printf("(%s:) *** the major version number of gfortran must be ranged from 4 to 9!\n",MYNAME);
	    exit(1);
	}

	/* find GNU_MINOR_VERSION_NUM */
	x_asprintf(&command,"%s --version | sed -n 1p | sed -n '/^[^0-9]*[0-9]\\.\\([0-9]\\).*/ s//\\1/p'",compiler_fullname);
	fp = popen(command, "r");
	if (fp == NULL) {
	    printf("Failed to get GNU_MINOR_VERSION_NUM\n" );
	    exit(1);
	}
	if (fscanf( fp, "%i", &GNU_MINOR_VERSION_NUM) == -1) {
	    fatal("Failed to read GNU_MINOR_VERSION_NUM from shell command");
	}
	pclose(fp);
	/* GNU gfortran version must be >= 4.4 */
	if (GNU_MAJOR_VERSION_NUM==4) {
	    if (GNU_MINOR_VERSION_NUM<4) {
		printf("(%s:) *** f90cache supports only version numbers >= 4.4!\n",MYNAME);
		exit(1);
	    }
	}

	x_asprintf(&GNU_preprocessor, "%s/cpp-%s -traditional-cpp",
		   compiler_path, GNU_VERSION);
	f90_compiler_type = GNU_GFC;
	free(compiler_path);
    } else if (strncmp(compiler_name, "ifort", 5) == 0) {
	f90_compiler_type = INTEL_IFC;
    } else {
	printf("(%s:) Fortran 90 compiler not supported: %s\n",MYNAME,compiler_name);
	fc_log("Unsupported compiler: %s\n", compiler_name);
	fatal("f90cache_driver");
    }
    free(compiler_name);

    /* we might be disabled */
    if (getenv("F90CACHE_DISABLE")) {
	fc_log("'f90cache' is disabled\n");
	failed();
    }

    /* process argument list, returning a new set of arguments for
       pre-processing in stripped_args */
    process_args(orig_args->argc, orig_args->argv);

    find_hash(stripped_args);

    /* if we can return from cache at this point then do */
    from_cache(1);

    if (getenv("F90CACHE_READONLY")) {
	fc_log("Read-only set - doing real compilation process\n");
	failed();
    }

    /* run real compiler, sending output to cache */
    to_cache(stripped_args);

    /* return from cache */
    from_cache(0);

    fc_log("Secondary from_cache failed\n");
    stats_update(STATS_ERROR);
    failed();
}

static void usage(void)
{
    printf("\n  %s, a Fortran 2008 compiler cache. Version %s\n", MYNAME, F90CACHE_VERSION);
    printf("  Édouard Canot, May 2019\n\n");

    printf("  Usage:\n");
    printf("     %s [cache options]\n", MYNAME);
    printf("     %s compiler [compile options]\n\n", MYNAME);

    printf("  Cache options:\n");
    printf("     -s               show statistics summary\n");
    printf("     -z               zero statistics\n");
    printf("     -c               run a cache cleanup\n");
    printf("     -C               clear the cache completely\n");
    printf("     -F <maxfiles>    set maximum files in cache\n");
    printf("     -M <maxsize>     set maximum size of cache (use G, M or K)\n");
    printf("     -h               this help page\n");
    printf("     -V               print version number\n\n");

    printf("  Supported compilers:\n");
    printf("     GNU    gfortran,  version 4.4 to 9\n");
    printf("     INTEL  ifort,     version 9 to 16\n\n");
}

/* the main program when not doing a compile */
static int f90cache_command(int argc, char *argv[])
{
    int c;
    size_t v;

    while ((c = getopt(argc, argv, "hszcCF:M:V")) != -1) {
	switch (c) {
	case 'V':
	    printf("%s version %s [from ccache-2.4 of Andrew Tridgell]\n", MYNAME, F90CACHE_VERSION);
	    printf("Édouard Canot, May 2019\n");
	    break;

	case 'h':
	    usage();
	    break;

	case 's':
	    stats_summary();
	    break;

	case 'c':
	    cleanup_all(cache_dir);
	    printf("Cleaned cache\n");
	    break;

	case 'C':
	    wipe_all(cache_dir);
	    printf("Cleared cache\n");
	    break;

	case 'z':
	    stats_zero();
	    printf("Statistics cleared\n");
	    break;

	case 'F':
	    v = atoi(optarg);
	    stats_set_limits(v, -1);
	    printf("Set cache file limit to %u\n", (unsigned)v);
	    break;

	case 'M':
	    v = value_units(optarg);
	    stats_set_limits(-1, v);
	    printf("Set cache size limit to %uk\n", (unsigned)v);
	    break;

	default:
	    usage();
	    exit(1);
	}
    }

    if (cache_dir_allocated)
	free(cache_dir);
    return 0;
}

int main(int argc, char *argv[])
{
    char *p;

    cache_dir = getenv("F90CACHE_DIR");
    if (!cache_dir) {
	x_asprintf(&cache_dir, "%s/.f90cache", get_home_directory());
	cache_dir_allocated = 1;
    }

    temp_dir = getenv("F90CACHE_TEMPDIR");
    if (!temp_dir) {
	temp_dir = cache_dir;
    }

    cache_logfile = getenv("F90CACHE_LOGFILE");

    /* the user might have set F90CACHE_UMASK */
    p = getenv("F90CACHE_UMASK");
    if (p) {
	mode_t mask;
	errno = 0;
	mask = strtol(p, NULL, 8);
	if (errno == 0) {
	    umask(mask);
	}
    }

    /* check if we are being invoked as "f90cache";
       may be via: "./f90cache" or "/opt/bin/f90cache", etc.
     */
    if (strlen(argv[0]) >= strlen(MYNAME) &&
	strcmp(argv[0] + strlen(argv[0]) - strlen(MYNAME), MYNAME) == 0) {
	if (argc < 2) {
	    if (cache_dir_allocated)
		free(cache_dir);
	    usage();
	    exit(1);
	}
	/* if the first argument isn't an option, then assume we are
	   being passed a compiler name and options */
	if (argv[1][0] == '-') {
	    return f90cache_command(argc, argv);
	}
    }

    /* make sure the cache dir exists */
    if (create_dir(cache_dir) != 0) {
	fprintf(stderr,"f90cache: failed to create cache dir: %s (%s)\n",
		cache_dir, strerror(errno));
	exit(1);
    }

    f90cache_driver(argc, argv);
    if (cache_dir_allocated)
	free(cache_dir);
    return 1;
}
