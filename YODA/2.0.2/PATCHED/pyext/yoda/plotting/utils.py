import yoda
import numpy as np
import math


def safeDiv(numer, denom):
    """Simple division method with implicit divByZero protection."""
    return np.divide(numer, denom,
                     out = np.where(numer != 0, np.nan, 1),
                     where = denom != 0)


def transpose(arr):
    """Helper function to transpose a matrix."""
    return np.array(list(map(list,zip(*arr))))


def rebinTo(xedges_probe, xedges_ref, yvals, yerrs):
    """Helper method to rebin yvals and yerrs to match shape of reference curve."""
    new_yvals = []; new_xerrs = [[],[]]; new_yerrs = [[],[]]
    refEdges = list(reversed(xedges_ref)); refEdge = refEdges.pop()
    norm = 0.0
    for i, x in enumerate(xedges_probe):
        if math.isclose(x, refEdge):
            if len(new_yvals) or not len(refEdges):
                # finish scaling previous bin
                if norm:  new_yvals[-1] /= norm
                new_yerrs[0][-1] *= new_yvals[-1] ** 2
                new_yerrs[1][-1] *= new_yvals[-1] ** 2
                if not len(refEdges):
                    break # we're done, skip overflow
            # add new bin
            new_yerrs[0].append( (yerrs[0][i]/yvals[i]) ** 2 if yvals[i] else 0. )
            new_yerrs[1].append( (yerrs[1][i]/yvals[i]) ** 2 if yvals[i] else 0. )
            wt = 0.5*(new_yerrs[0][-1] + new_yerrs[1][-1])
            if not wt:  wt = 1.
            new_yvals.append(yvals[i]/wt)
            err = 0.5*abs(refEdges[-1] - refEdge)
            new_xerrs[0].append(err)
            new_xerrs[1].append(err)
            norm = 1.0/wt
            # set new upper edge
            refEdge = refEdges.pop()
        elif not len(new_yvals) and x < refEdge:
            continue # underflow
        # otherwise merge
        relEsqlo = (yerrs[0][i]/yvals[i]) ** 2 if yvals[i] else 0.
        relEsqhi = (yerrs[1][i]/yvals[i]) ** 2 if yvals[i] else 0.
        wt = 0.5*(relEsqlo + relEsqhi)
        if not wt:  wt = 1.0
        new_yvals[-1] += yvals[i]/wt
        new_yerrs[0][-1] += relEsqlo
        new_yerrs[1][-1] += relEsqhi
        norm += 1.0/wt

    return np.array(new_yvals), np.array(new_xerrs), np.sqrt(np.array(new_yerrs))


def pad(xprobe, xref, yvals, *args):
    """Helper method to pad yvals and yerrs with NaNs where they have
       fewer elements than the reference."""
    if xprobe.shape == xref.shape:
        return yvals, *args
    padded_yvals = np.array([ yvals[x==xprobe][0] if x in xprobe else np.nan for x in xref ])
    args = [ np.array([ [errs[x==xprobe][0] if x in xprobe else \
                         np.nan for x in xref ] for errs in arg ]) for arg in args ]
    return (padded_yvals, *args)


def reshape(ref_vals, ref_edges, ao, force_rebin = False):
    """Helper method to reshape yvals and yerrs to match shape of reference curve.
       If the probe has fewer elements than the reference, pad using NaNs."""
    xprobe = ao.xVals()
    if force_rebin and xprobe.shape > ref_vals.shape:
        xedges = np.append(ao.xMins(), max(ao.xMaxs()))
        return rebinTo(xedges, ref_edges, ao.yVals(), transpose(ao.yErrs()))
    if xprobe.shape >= ref_vals.shape:
        return ao.yVals(), transpose(ao.xErrs()), transpose(ao.yErrs())
    return pad(xprobe, ref_vals, ao.yVals(), transpose(ao.xErrs()), transpose(ao.yErrs()))


def selectValRange(ao1, ao2, xmin = None, xmax = None):
    """Returns vectors of y-values and average uncertainties for
       two AOs, subject to an optional constraint on the x-range."""
    if ao1.xVals().shape != ao2.xVals().shape:
        # TODO: attempt to achieve homogenous binning here?
        return [], [], [], []
    indices = [ i for i,p in enumerate(ao1.points()) if p.x() >= xmin and p.x() <= xmax ]
    yvals1 = [ ao1.point(i).y() for i in indices ]
    yvals2 = [ ao2.point(i).y() for i in indices ]
    yerrs1 = [ ao1.point(i).yErrAvg() for i in indices ]
    yerrs2 = [ ao2.point(i).yErrAvg() for i in indices ]
    return yvals1, yvals2, yerrs1, yerrs2


def mkPlotFriendlyScatter(ao,includeOverflows=False,includeMaskedBins=True,errorPattern=""):
    """Converts non-scatter AOs to scatters. If the AO has masked bins,
       the corresponding indices are stored in the metadata.If the AO has
       discrete binning, the edge labels are also stored in the metadata."""

    origT = str(type(ao))
    rtn = ao.mkScatter(ao.path(), errorPattern,
                       includeOverflows=includeOverflows,
                       includeMaskedBins=includeMaskedBins) if 'BinnedEstimate' in origT else \
          ao.mkScatter(ao.path(), binwidthdiv=True, useFocus=False,
                       includeOverflows=includeOverflows,
                       includeMaskedBins=includeMaskedBins) if 'Binned' in origT else \
          ao.mkScatter(ao.path(), errorPattern) if 'Estimate' in origT else ao.mkScatter(ao.path())
    # promote S1D to S2D
    if rtn.type() == "Scatter1D":
        s2D = yoda.Scatter2D([(1.0, rtn.point(0).x(), 0.5, 0.5, *rtn.point(0).xErrs())], rtn.path())
        s2D.setAnnotation('DummyXaxis', 1)
        if rtn.hasAnnotation("IsRef"):
            s2D.setAnnotation('IsRef', 1)
        rtn = s2D
    if hasattr(ao, 'binDim'):
        # if there are masked bins, find them and set value to NaN
        if includeMaskedBins and len(ao.maskedBins()) > 0:
            maskIdx = -1
            for b in ao.bins(False, True):
                maskIdx += 1
                if b.isMasked():
                    rtn.point(maskIdx).setVal(ao.dim()-1, np.nan)

        # if there are discrete axes, set a +/-0.5 dummy uncertainty
        # and set custom tick-mark labels
        for i, axis in enumerate(ao.axisConfig().split(',')):
            if axis != 'd':
                # add dummy uncertainty
                for p in rtn.points():
                    p.setErr(i, 0.5)
                # decorate with custom labels
                if i < 3 and rtn.hasAnnotation('EdgesA%d' % (i+1)):
                    ALPHA = 'XYZ'
                    labels = rtn.annotation('EdgesA%d' % (i+1))
                    anno = '\t'.join([ '%d\t%s' % x for x in list(zip(rtn.vals(i),labels)) ])
                    rtn.setAnnotation(ALPHA[i]+'CustomMajorTicks', anno)
    return rtn


def linear_rebin(arr, rebin):
    """Helper function to rebin input array by integer factor rebin.
       Bin values are added linearly."""
    newlen = len(arr) // rebin
    shape = (newlen,rebin,2) if len(arr.shape) > 1 else (newlen,rebin)
    rtn = np.sum(np.resize(arr,shape), axis=1)
    res = len(arr) % rebin # check residuals if binning uneven
    if res:  rtn[-1] += np.sum(arr[-res:],axis=0)
    return rtn


def quad_rebin(arr, rebin):
    """Helper function to rebin input array by integer factor rebin.
       Bin values are added in quadrature."""
    return  np.sqrt(linear_rebin(arr ** 2, rebin))


def scatter_rebin(ao, rebin):
    """Helper function to attempt an on-the-fly rebinning
       of the input Scatter2D ao by an integer factor.
       This operation might not be well defined if the
       points are overlapping or non-adjacent."""
    if rebin < 2 or ao.dim() != 2:
        return ao

    oldmins = ao.xMins()
    oldmax = ao.xMax()
    oldw = np.append(oldmins[1:], [oldmax]) - oldmins
    old2w = np.array([oldw,oldw]).transpose()
    # determine new x-values
    xmin = np.resize(oldmins,(len(oldmins)//rebin,rebin))[:,0]
    xmax = np.append(xmin[1:], [oldmax])
    xerrs = 0.5*(xmax - xmin)
    xvals = xmin + xerrs
    # determine new y-values (assume bin width division)
    neww = linear_rebin(oldw, rebin)
    new2w = np.array([neww,neww]).transpose()
    yvals = linear_rebin(oldw*ao.yVals(), rebin) / neww
    yerrs = quad_rebin(old2w*ao.yErrs(), rebin) / new2w
    # create new Scatter2D
    rtn = yoda.Scatter2D()
    for k in ao.annotations():
        rtn.setAnnotation(k, ao.annotation(k))
    rtn.addPoints(zip(xvals, yvals, xerrs, xerrs, *yerrs.transpose()))
    return rtn


def splitRange(rangeMin, rangeMax, n, isLog):
    """Helper function to determine delimeters that
       split a lin/log range into n equal patches."""
    if n == 1:  return None
    lo = np.log10(rangeMin) if isLog else rangeMin
    hi = np.log10(rangeMax) if isLog else rangeMax
    delims = np.array([
      ((n-1-idx)*lo + (1+idx)*hi)/float(n) for idx in range(n-1)
    ])
    if isLog:
        delims = 10 ** delims
    return delims[0] if n == 2 else delims


def legendDefaults(refAO, xlims, ylims, logx, logy):
    """Helper function to find suitable default x/y positions,
       anchor and alignment for a legend, given reference curve."""

    yvals = refAO.yVals()
    # max/min of the y-axis range
    yax_min, yax_max = ylims
    # work out middle of y-axis range
    ymid = splitRange(*ylims, 2, logy)
    anchor = 'upper right'; align = 'r'
    xypos = (1.00,0.97)
    if len(yvals) == 1: # only one point on the canvas
        if yvals[0] > ymid: # mv legend down
            anchor = anchor.replace('upper', 'lower')
            xypos = (1.00,0.03)
        return *xypos, anchor, align
    elif len(yvals) == 2: # only two points on the canvas
        if np.sum(yvals > ymid) != 1:  # 2-by-2 matrix is diagonal
            if yvals[-1] > ymid: # mv legend to the left
                anchor = anchor.replace('right', 'left')
                align = 'l'
                xypos = (0.00,0.97)
        else: # mv legend below curve
            # ToDo: extend y-axis range?
            xypos = (1.00,0.03)
        return *xypos, anchor, align
    # split x-range into thirds
    xlo, xhi = splitRange(*xlims, 3, logx)
    xvals = refAO.xVals()
    # split yvals into three segements along x-range
    y_segs = [
      yvals[np.where(xvals<xlo)],
      yvals[np.where(np.logical_and(xlo<xvals, xvals<xhi))],
      yvals[np.where(xhi<xvals)],
    ]
    # ensure there's at least one element in each segment
    if not len(y_segs[0]):  y_segs[0] = np.append(y_segs[0], yvals[0])
    if not len(y_segs[1]):
      xmid =  splitRange(*xlims, 2, logx)
      y_segs[1] = np.append(y_segs[1], yvals[np.abs(xvals-xmid).argmin()])
    if not len(y_segs[2]):  y_segs[2] = np.append(y_segs[2], yvals[-1])
    # calculate centroids and their widths in each segment
    ymeans = np.array([ arr.mean(axis=0) for arr in y_segs ])
    ystdev = np.array([ arr.std(axis=0)  for arr in y_segs ])
    # split y-range into thirds
    ylo, yhi = splitRange(*ylims, 3, logy)
    # locate white space as Boolean 3-by-3 matrix
    res = np.array((
      (ymeans + ystdev) < yhi,
      np.logical_or((ymeans + ystdev) < ylo, yhi < (ymeans-ystdev)),
      ylo < (ymeans - ystdev),
    ))
    # apply Gaussian smoothing and determine the 'emptiest' patch
    kernel = np.array([0.5,1.0,0.5])
    loc = np.abs(
        np.apply_along_axis(lambda x: np.convolve(x, kernel, mode='same'), 0, res)
      + np.apply_along_axis(lambda x: np.convolve(x, kernel, mode='same'), 1, res)
    ).argmax()
    # translate best index into position, orientation & alignment
    anchor = [
      'upper left', 'upper center', 'upper right',
      'center left', 'center', 'center right',
      'lower left', 'lower center', 'lower right',
    ][loc]
    xypos = [
      (0.00,0.97), (0.50,0.97), (1.00,0.97),
      (0.00,0.50), (0.50,0.50), (1.00,0.50),
      (0.00,0.03), (0.50,0.00), (1.00,0.03),
    ][loc]
    if not loc in [2,5,8]:
        align = 'l'
    return (*xypos, anchor, align)
