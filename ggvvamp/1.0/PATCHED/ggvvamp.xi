// template instantiations
//#ifdef ARBITRARY_PRECISION
//template class ggvvamp<cln::cl_F>;
//#endif
#ifdef QUAD_PRECISION
template class ggvvamp<quadtype>;
#endif
#ifdef DOUBLE_PRECISION
template class ggvvamp<double>;
#endif
