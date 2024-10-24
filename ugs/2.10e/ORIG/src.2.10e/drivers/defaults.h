/* application defaults */

/* resources available for Core-Widget */

static char *fallbackResources[] =
{
  "*width:        768",
  "*height:       585",
  "*background:   black",
  "*title:        UGS on X Window System",
  "*iconName:     UGS",
  "*BackingStore: Always",
  0
};

/* resources not available for Core-Widget */

typedef struct {
  XColor  color[9];
  String  fontpattern[256];
} AppData, *AppDataPtr;

static AppData app;

#define XtNwhite   "white"
#define XtCWhite   "White"
#define XtNred     "red"
#define XtCRed     "Red"
#define XtNgreen   "green"
#define XtCGreen   "Green"
#define XtNblue    "blue"
#define XtCBlue    "Blue"
#define XtNyellow  "yellow"
#define XtCYellow  "Yellow"
#define XtNmagenta "magenta"
#define XtCMagenta "Magenta"
#define XtNcyan    "cyan"
#define XtCCyan    "Cyan"
#define XtNfontPattern "fontPattern"
#define XtCFontPattern "FontPattern"

static XtResource resources[] = {
  {
    XtNwhite,
    XtCWhite,
    XtRPixel,
    sizeof(Pixel),
    XtOffset(AppDataPtr, color[1].pixel),
    XtRString,
    "White",
  },
  {
    XtNred,
    XtCRed,
    XtRPixel,
    sizeof(Pixel),
    XtOffset(AppDataPtr, color[2].pixel),
    XtRString,
    "Red",
  },
  {
    XtNgreen,
    XtCGreen,
    XtRPixel,
    sizeof(Pixel),
    XtOffset(AppDataPtr, color[3].pixel),
    XtRString,
    "Green",
  },
  {
    XtNblue,
    XtCBlue,
    XtRPixel,
    sizeof(Pixel),
    XtOffset(AppDataPtr, color[4].pixel),
    XtRString,
    "Blue",
  },
  {
    XtNyellow,
    XtCYellow,
    XtRPixel,
    sizeof(Pixel),
    XtOffset(AppDataPtr, color[5].pixel),
    XtRString,
    "Yellow",
  },
  {
    XtNmagenta,
    XtCMagenta,
    XtRPixel,
    sizeof(Pixel),
    XtOffset(AppDataPtr, color[6].pixel),
    XtRString,
    "Magenta",
  },
  {
    XtNcyan,
    XtCCyan,
    XtRPixel,
    sizeof(Pixel),
    XtOffset(AppDataPtr, color[7].pixel),
    XtRString,
    "Cyan",
  },
  {
    XtNfontPattern,
    XtCFontPattern,
    XtRString,
    sizeof(int),
    XtOffset(AppDataPtr, fontpattern),
    XtRString,
    "-adobe-helvetica-medium-r-normal--*-*-75-75-*",
  },
};
