library(colorRamps)

#monochrome
#lightness increase kept constant for all scales
#scale with hue also inreases in saturation/chroma
greyscale <- colorRampPalette(c("#F7F7F7", "#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252", "#252525"), space="Lab")
bluescale <- colorRampPalette(c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"), space="Lab")
greenscale <- colorRampPalette(c("#EDF8E9", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32"), space="Lab")

#hue/lightness
YellowRed <- colorRampPalette(c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#B10026"), space="Lab")

#spectral
Spectral <- function(n) rainbow(n, start=0, end=0.75)
matlabSpectral <- function(n) rev(matlab.like(n))

#diverging
RedWhiteBlue <- colorRampPalette(c("#B2182B", "#EF8A62", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#67A9CF", "#2166AC"), space="Lab")

## Here's a the set of colours we'll use:
colour_schemes <- function() {
  list(greyscale=greyscale,
       bluescale=bluescale,
       YellowRed=YellowRed,
       RedWhiteBlue=RedWhiteBlue,
       Spectral=Spectral,
       greenscale=greenscale,
       matlabSpectral=matlabSpectral)
}

