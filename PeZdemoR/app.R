# PeZdemoR
# (c) &copy; 2016-2017 Peter Squires, `DiYZer Research Group`, ECE Dept, University of Victoria
#
# Squires, Peter. Visualizing DSP Concepts on the Web Using the R-Language shiny Package. 
#    University of Victoria, 2017. https://dspace.library.uvic.ca/handle/1828/7815
#
# options(shiny.sanitize.errors = TRUE) # http://shiny.rstudio.com/articles/sanitize-errors.html
#options(rgl.useNULL = TRUE)
memory.limit(size = 1e+05)

# ADD-ON PACKAGES **************************************************** ----
library(shiny)
library(shinyBS) # bsModal, bsButton, bsCollapse/ bsCollapsePanel, updateButton, createAlert/ closeAlert
library(shinyjs) # reset, info, delay, show, hide, disable, onevent, hidden
library(signal) # fir1, fir2, butter, cheby1, cheby2, ellip, butterord, cheby1ord, ellipord, remez, spencerFilter, sgolay, bilinear, sftrans, freqz, unwrap, filter, fftfilt, impz, bartlett, blackman, boxcar, chebwin, flattopwin, gausswin, hanning, hamming, triang, Zpg, Arma, Ma, sgolay, specgram
library(pracma) # zeros, fliplr/ flipud, ifft, Toeplitz, inv, isempty, polar, meshgrid, polyval, fftshift, Poly, polymul/ conv, str2num, findpeaks, linspace
library(MASS) # fractions # _Modern Applied Statistics with S_, Venables, W.N. and Ripley, B.D. (2002)
library(colourpicker) # colourInput
#library(colorspace) # rainbow_hcl, diverge_hcl
#library(RColorBrewer) # brewer.pal
library(threejs) # scatterplotThreeOutput/ renderScatterplotThree, scatterplot3js
library(rgl) # rglwidgetOutput, renderRglwidget, persp3d, plot3d, surface3d, rglwidget, playwidget, scene3d, rgl.close, clear3d
library(knitr) # kable, include_graphics; Note: auto-calculation of image-output-width requires the `png` package (for PNG images)
library(R.matlab) # readMat, writeMat
library(yaml) # yaml.load_file, as.yaml
library(rmarkdown) # render, pandoc_available
library(markdown) # markdownToHTML
library(devtools) # session_info
library(png)
library(webshot) # appshot
library(audio) # play
library(tuneR) # readWave, readMP3, writeWave, noise, pulse, sawtooth, silence, sine, square, periodogram
library(tools) # file_ext, file_path_sans_ext
library(rwt) # makesig
# library(matlab) # meshgrid (3D)
# library(Matrix) # sparseMatrix
library(astsa) # data("soi") # Southern-Oscillation Index (SOI), 453 months, 1950-1987 # data("nyse") # New York Stock Exchange, 2/Feb/84 to 31/Dec/91
# library(seewave) # localpeaks, spec, meanspec
library(rintrojs) # introBox # wrapper for the [Intro.js library](http://www.introjs.com)

# Constant-Symbol Definitions ---------------------------------------- ----

# to allow for engineering-style imaginary-numbers/ assignments/ references:
assign("j", 0 + (0 + 1i))
assign("1j", 0 + (0 + 1i))
assign("Infi", complex(1, 0, Inf))
assign("Infj", complex(1, 0, Inf))

assign("eps", .Machine$double.neg.eps) # small positive floating-point number x, such that: 1-x != 1; typically, 1.110223e-16
assign("twoeps", .Machine$double.eps) # small positive floating-point number x, such that: 1+x != 1; typically, 2.220446e-16
assign("fpmaxx", .Machine$double.xmax) # largest normalized floating-point number; typically, 1.797693e+308

# CUSTOMIZATION ****************************************************** ----
scalePlotsToVerticalHeight <- FALSE # TRUE # recommended `FALSE` for typical tablet-usage (i.e. rotated)
verticalHeightOfPlots <- "80vh" # =80% of browser's full vertical-height
AppBackgroundColor <- "#D4D0C8" # http://www.w3schools.com/cssref/css_colors.asp
AppBackgroundImage <- "cubes_light_texture_background_hd-wallpaper-48413.jpg" # http://hd-walls.com/wp-content/uploads/2016/02/10/22/gray_cubes_light_texture_background_hd-wallpaper-48413.jpg
PlotBackgroundColor <- "white"
MyColourForUnstableSystem <- "#FF9999"
NavBarPageBackgroundColor <- NULL
WellPanelBackgroundColor <- NULL
MainSidePanelBackgroundColor <- NULL
TabPanelBackgroundColor <- NULL
SubTabPanelBackgroundColor <- NULL
AvailableThemes <-
  c(
    NULL, # equals the default "vanilla-style" bootstrap-theme
    "cerulean", # https://bootswatch.com/
    "cosmo",
    "flatly",
    "lumen",
    "paper",
    "readable",
    "spacelab",
    "united",
    "yeti" #  # this 10th-slot seems to be the first to always be "randomly" chosen with any new freshly-started RStudio process
    # ,"creative" # okay? # https://startbootstrap.com/
    # ,"new-age" # okay?
    # ,"clean-blog" # okay?
    # ,"sb-admin"
    # ,"scrolling-nav"
    # ,"freelancer"
    # ,"grayscale"
    # ,"business-casual"
    # ,"sb-admin-2"
    # ,"stylish-portfolio"
    # ,"simple-sidebar"
    # ,"landing-page"
    # ,"small-business"
    # ,"modern-business"
    # ,"agency"
    # ,"logo-nav"
    # ,"business-frontpage"
  )
MyChosenTheme <- sample(AvailableThemes, 1L)
MyAppNameAndTheme <- paste(MyChosenTheme, "<b>P</b>e<b>Z</b>")
mySidebarWidth <- 5L

# FUNCTIONS ********************************************************** ----
sinc <- function(x) {
  ifelse(x == 0, 1, sin(pi * x) / (pi * x))  # isTRUE(all.equal( #
}

stripImagZero <- function(x) {
  if (max(abs(Im(x)),na.rm=TRUE) < .Machine$double.eps ^ 0.5) {
    return(Re(x))
  }
  else {
    return(x)
  }
}

countZeroDigitsRightOfDecimalPoint <-
  function(x, tol = .Machine$double.eps ^ 0.5) {
    # http://stackoverflow.com/questions/35553244/count-leading-zeros-between-the-decimal-point-and-first-nonzero-digit
    if (((abs(Re(x)) < tol) &&
         (abs(Im(x)) < tol)) || (is.infinite(x))) {
      return(0L)
    }
    else if (abs(Im(x)) < tol) {
      x <- abs(Re(x))
      if (((x - floor(x)) < tol) || abs(x) < tol) {
        return(0L)
      }
      y <- -log10(x - floor(x))
      return(floor(y) - (y %% 1L < tol))
    }
    else {
      xReal <- abs(Re(x))
      if (((xReal - floor(xReal)) < tol) || (xReal < tol)) {
        return(0L)
      }
      yReal <- -log10(xReal - floor(xReal))
      xImag <- abs(Im(x))
      if (((xImag - floor(xImag)) < tol) || (xImag < tol)) {
        return(0L)
      }
      yImag <- -log10(xImag - floor(xImag))
      return(min(4L, max(c(
        floor(yReal) - (yReal %% 1L < tol),
        floor(yImag) -
          (yImag %% 1L < tol)
      ), na.rm = TRUE), na.rm = TRUE))
    }
  }

unWrap <- function(p, tol = (pi - eps)) {
  # https://www.dsprelated.com/freebooks/sasp/Matlab_listing_unwrap_m.html
  #   or https://ccrma.stanford.edu/~jos/sasp/Matlab_listing_unwrap_m.html
  N <- length(p)
  up <- rep(0, times = N)
  pm1 <- p[1]
  up[1] <- pm1
  po <- 0
  thr <- pi - eps
  twopi <- 2 * pi
  for (i in 2L:N) {
    cp <- p[i] + po
    dp <- cp - pm1
    pm1 <- cp
    if (dp > thr) {
      while (dp > thr) {
        po <- po - twopi
        dp <- dp - twopi
      }
    }
    if (dp < -thr) {
      while (dp < -thr) {
        po <- po + twopi
        dp <- dp + twopi
      }
    }
    cp <- p[i] + po
    pm1 <- cp
    up[i] <- cp
  }
  return(up)
}

myfreqz <- function(B,
                    A,
                    N = 1024L,
                    whole = 1L,
                    fs = 1) {
  # https://ccrma.stanford.edu/~jos/fp/Frequency_Response_Matlab.html
  # "Introduction to Digital Filters with Audio Applications", Julius O. Smith III, (Sept/2007)
  na <- length(A)
  nb <- length(B)
  if ((is.complex(B)) || (is.complex(A))) {
    whole <- 1L
  }
  Nf <- 2L * N
  if (whole == 1L) {
    Nf <- N
  }
  w <- (2 * pi * fs * (0L:(Nf - 1L)) / Nf)
  H <-
    fft(c(B, pracma::zeros(1L, Nf - nb))) / fft(c(A, pracma::zeros(1L,
                                                                  Nf - na)))
  if (whole == 1L) {
    w <- w[1L:N]
    H <- H[1L:N]
  }
  if (fs == 1) {
    flab <- "Frequency (cycles/sample)"
  }
  else {
    flab <- "Frequency (Hz)"
  }
  par(mfrow = c(2, 1))
  plot(
    c(0:(N - 1)) * fs / N,
    20 * log10(Mod(H)),
    type = "l",
    col = "blue",
    xlab = flab,
    ylab = "Magnitude (dB)"
  )
  grid(col = "grey")
  abline(h = 0L, col = "black")
  abline(v = 0L, col = "black")
  plot(
    c(0L:(N - 1L)) * fs / N,
    Arg(H),
    type = "l",
    col = "blue",
    xlab = flab,
    ylab = "Phase"
  )
  grid(col = "grey")
  abline(h = 0L, col = "black")
  abline(v = 0L, col = "black")
  return(list(H = H, w = w))
}

myfindpeaks <-
  function(x,
           nups = 1L,
           ndowns = nups,
           zero = "0",
           peakpat = NULL,
           minpeakheight = -Inf,
           minpeakdistance = 1,
           threshold = 0,
           npeaks = 0L,
           sortstr = FALSE) {
    # the following is taken from the `pracma` package:
    stopifnot(is.vector(x, mode = "numeric"))
    if (!zero %in% c("0", "+", "-"))
      stop("Argument 'zero' can only be '0', '+', or '-'.")
    xc <- paste(as.character(sign(diff(x))), collapse = "")
    xc <- gsub("1", "+", gsub("-1", "-", xc))
    if (zero != "0")
      xc <- gsub("0", zero, xc)
    if (is.null(peakpat)) {
      peakpat <- sprintf("[+]{%d,}[-]{%d,}", nups, ndowns)
    }
    rc <- gregexpr(peakpat, xc)[[1]]
    if (rc[1] < 0L)
      return(NULL)
    x1 <- rc
    x2 <- rc + attr(rc, "match.length")
    attributes(x1) <- NULL
    attributes(x2) <- NULL
    n <- length(x1)
    xv <- xp <- numeric(n)
    for (i in 1L:n) {
      xp[i] <- which.max(x[x1[i]:x2[i]]) + x1[i] - 1L
      xv[i] <- x[xp[i]]
    }
    inds <-
      which(xv >= minpeakheight & xv - pmax(x[x1], x[x2]) >= threshold)
    X <- cbind(xv[inds], xp[inds], x1[inds], x2[inds])
    if (minpeakdistance < 1L)
      warning("Handling 'minpeakdistance < 1' is logically not possible.")
    if (sortstr || minpeakdistance > 1L) {
      sl <- sort.list(X[, 1], na.last = NA, decreasing = TRUE)
      X <- X[sl, , drop = FALSE]
    }
    if (length(X) == 0L)
      return(c())
    if (minpeakdistance > 1) {
      no_peaks <- nrow(X)
      badpeaks <- rep(FALSE, no_peaks)
      for (i in 1L:no_peaks) {
        ipos <- X[i, 2]
        if (!badpeaks[i]) {
          dpos <- abs(ipos - X[, 2])
          badpeaks <- badpeaks | (dpos > 0L & dpos < minpeakdistance)
        }
      }
      X <- X[!badpeaks,]
    }
    if (is.vector(X)) { # <<< different from original `pracma` package's function-code
      if (npeaks > 0L && npeaks < length(X) / 4) { # <<< different from original `pracma` package's function-code
        X <- X[1L:npeaks, , drop = FALSE]
      }
    }
    else {
      if (npeaks > 0L && npeaks < nrow(X)) { # <<< different from original `pracma` package's function-code
        X <- X[1L:npeaks, , drop = FALSE]
      }
    }
    return(X)
  }

stabilityCheck <- function(A) {
  # https://www.dsprelated.com/freebooks/filters/Pole_Zero_Analysis_I.html
  N <- length(A) - 1L
  stable <- 1L
  for (i in seq(N, 1L, by = -1L)) {
    rci <- A[i + 1]
    if (Mod(rci) >= 1) {
      stable <- 0L
      break
    }
    A <- (A[1L:i] - rci * A[seq((i + 1L), 2L, by = -1L)]) / (1L - rci ^ 2)
  }
  return(stable)
}

plot_imp <-
  function(handleshn,
           hnimag,
           handlespoleloc,
           input,
           output) {
    # taken mostly from `\private\` subdirectory of `pezdemo.m` app of `SP-First` fame, https://github.com/DeepHorizons/spfirst/tree/master/spfirst/pezdemo/private
    pt <- min(input$maxLengthImpulseResponse, length(handleshn))
    x <- 0L:(pt - 1L)
    tempi <- Im(handleshn)
    xx <- matrix(0, nrow = 1L, ncol = (3L * pt))
    xx[seq(from = 1L, to = 3L * pt, by = 3L)] <- x
    xx[seq(from = 2L, to = 3L * pt, by = 3L)] <- x
    xx[seq(from = 3L, to = 3L * pt, by = 3L)] <- NaN
    handlestol <- 1e-04
    if (Mod(hnimag[2]) < handlestol) {
      plot(
        x,
        if (any(is.infinite(Re(handleshn)))) {
          1e+12
        }
        else {
          Re(handleshn)
        },
        type = "h",
        xlim = c(0, pt),
        ylim = c(max(c(
          -1e+12, min(c(0,
                        Re(handleshn)), na.rm = TRUE)
        ), na.rm = TRUE), min(c(
          1e+12,
          max(c(0, Re(handleshn)), na.rm = TRUE)
        ), na.rm = TRUE)),
        xlab = "n",
        ylab = "h[n]",
        main = "Impulse-Response",
        col = input$ForegroundColor,
        lwd = input$LineWidth
      )
      grid(col = input$grcolor)
      abline(h = 0L)
      abline(v = 0L)
      output$system_real <- renderUI({
        tags$span(style = paste0("color:", input$ForegroundColor),
                  "real-valued")
      })
    }
    else {
      yy <- matrix(0, nrow = 1L, ncol = 3L * pt)
      yy[seq(from = 2L,
             to = 3L * pt,
             by = 3L)] <- tempi
      yy[seq(from = 3L,
             to = 3L * pt,
             by = 3L)] <- NaN
      plot(
        x,
        Re(handleshn), # tempi,
        type = "p",
        pch = 21,
        bg = input$BackgroundColor,
        xlim = c(0, pt),
        # ylim = c(min(c(-1, yy, tempi), na.rm = TRUE) *
        #            1.2, max(c(1, yy, tempi), na.rm = TRUE) * 1.2),
        ylim = c(min(c(-1, yy, Re(handleshn)), na.rm = TRUE) *
                   1.2, max(c(1, yy, Re(handleshn)), na.rm = TRUE) * 1.2),
        col = "transparent", # "magenta",
        lwd = input$LineWidth,
        xlab = "n",
        ylab = "h[n]",
        main = "Impulse-Response (Real-Part)"
      )
      grid(col = input$grcolor)
      abline(h = 0L)
      abline(v = 0L)
      # if (input$showLegend)
      #   legend(
      #     "topright",
      #     c("real", "imag"),
      #     col = c(input$ForegroundColor,
      #             "magenta"),
      #     lty = c("solid", "dashed"),
      #     bty = "n",
      #     cex = 0.8
      #   )
      # lines(
      #   type = "h",
      #   x,
      #   tempi,
      #   col = "magenta",
      #   lwd = input$LineWidth *
      #     2/3,
      #   lty = "dashed"
      # )
      output$system_real <- renderUI({
        tags$span(style = "color:magenta",
                  "[imaginary-parts discarded in coercion]")
      })
    }
    yy <- matrix(0, nrow = 1L, ncol = 3L * pt)
    yy[seq(from = 2L, to = 3L * pt, by = 3L)] <- Re(handleshn)
    yy[seq(from = 3L, to = 3L * pt, by = 3L)] <- NaN
    lines(
      x,
      Re(handleshn),
      type = "h",
      col = input$ForegroundColor,
      lwd = input$LineWidth
    )
    lines(
      x,
      Re(handleshn),
      col = "grey",
      lwd = input$LineWidth * 2/3,
      lty = "dashed"
    )
    points(
      x,
      Re(handleshn),
      pch = 21,
      bg = input$BackgroundColor,
      col = input$ForegroundColor,
      lwd = input$LineWidth
    )
  }

plot_step <-
  function(handleshnu,
           hnimag,
           handlespoleloc,
           input,
           output) {
    # taken mostly from `\private\` subdirectory of `pezdemo.m` app of `SP-First` fame, https://github.com/DeepHorizons/spfirst/tree/master/spfirst/pezdemo/private
    pt <- min(input$maxLengthImpulseResponse, length(handleshnu))
    x <- 0L:(pt - 1L)
    tempi <- Im(handleshnu)
    xx <- matrix(0L, nrow = 1L, ncol = (3L * pt))
    xx[seq(from = 1L, to = 3L * pt, by = 3L)] <- x
    xx[seq(from = 2L, to = 3L * pt, by = 3L)] <- x
    xx[seq(from = 3L, to = 3L * pt, by = 3L)] <- NaN
    handlestol <- 1e-04
    # if (Mod(hnimag[2]) < handlestol) {
      plot(
        x,
        if (any(is.infinite(Re(handleshnu)))) {
          1e+12
        }
        else {
          Re(handleshnu)
        },
        type = "s", # "s" for stair-steps, # "h", # "h" for histogram-like spikes
        xlim = c(0, pt),
        ylim = c(max(c(
          -1e+12, min(c(0,
                        Re(handleshnu)), na.rm = TRUE)
        ), na.rm = TRUE), min(c(
          1e+12,
          max(c(0, Re(handleshnu)), na.rm = TRUE)
        ), na.rm = TRUE)),
        xlab = "n",
        ylab = "hu[n]",
        main = "Unit-Step Response",
        col = input$ForegroundColor,
        lwd = input$LineWidth
      )
      grid(col = input$grcolor)
      abline(h = 0L)
      abline(v = 0L)
      text(pt-1L,Re(handleshnu[pt]),labels=round(Re(handleshnu[pt]),3L),pos=4L) # steady-state value
      # output$system_real <- renderUI({
      #   tags$span(style = paste0("color:", input$ForegroundColor),
      #             "real-valued")
      # })
    # }
    # else {
    #   yy <- matrix(0, nrow = 1L, ncol = 3L * pt)
    #   yy[seq(from = 2L,
    #          to = 3L * pt,
    #          by = 3L)] <- tempi
    #   yy[seq(from = 3L,
    #          to = 3L * pt,
    #          by = 3L)] <- NaN
    #   plot(
    #     x,
    #     tempi,
    #     type = "p",
    #     pch = 21L,
    #     bg = input$BackgroundColor,
    #     xlim = c(0L, pt),
    #     ylim = c(min(c(-1L, yy, tempi), na.rm = TRUE) *
    #                1.2, max(c(1L, yy, tempi), na.rm = TRUE) * 1.2),
    #     col = "magenta",
    #     lwd = input$LineWidth,
    #     xlab = "n",
    #     ylab = "h[n]",
    #     main = "Unit-Step Response"
    #   )
    #   grid(col = input$grcolor)
    #   abline(h = 0L)
    #   abline(v = 0L)
    #   if (input$showLegend)
    #     legend(
    #       "topright",
    #       c("real", "imag"),
    #       col = c(input$ForegroundColor,
    #               "magenta"),
    #       lty = c("solid", "dashed"),
    #       bty = "n",
    #       cex = 0.8
    #     )
    #   lines(
    #     type = "h",
    #     x,
    #     tempi,
    #     col = "magenta",
    #     lwd = input$LineWidth *
    #       2/3,
    #     lty = "dashed"
    #   )
    #   output$system_real <- renderUI({
    #     tags$span(style = "color:magenta",
    #               "[has imaginary-components (Note: not properly plotted here)]")
    #   })
    # }
    yy <- matrix(0, nrow = 1L, ncol = 3L * pt)
    yy[seq(from = 2L, to = 3L * pt, by = 3L)] <- Re(handleshnu)
    yy[seq(from = 3L, to = 3L * pt, by = 3L)] <- NaN
    lines(
      x,
      Re(handleshnu),
      type = "h",
      col = input$ForegroundColor,
      lwd = input$LineWidth
    )
    lines(
      x,
      Re(handleshnu),
      col = "grey",
      lwd = input$LineWidth * 2/3,
      lty = "dashed"
    )
    points(
      x,
      Re(handleshnu),
      pch = 21,
      bg = input$BackgroundColor,
      col = input$ForegroundColor,
      lwd = input$LineWidth
    )
  }

# https://www.r-bloggers.com/identifying-the-os-from-r/
# June/2015, by 'Will'
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

# USER-INTERFACE ***************************************************** ----
ui <-
  shinyUI(
    fluidPage(
      id = "myMainFluidPage",
      if (!is.null(AppBackgroundImage)) {
        tags$style(paste0(
          "body {background-image: url(\"",
          AppBackgroundImage,
          "\");}"
        ))
      },
      
      useShinyjs(),
      rintrojs::introjsUI(),
      
      tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"), # https://groups.google.com/forum/#!topic/shiny-discuss/9faA5OUm2Hc
      theme = paste0(MyChosenTheme, ".css"),
      tags$head(tags$style(".navbar .navbar-header {float: right}")),
      tags$head(
        tags$script(
          # http://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
'
var dimension = [0, 0];
$(document).on("shiny:connected", function(e) {
dimension[0] = window.innerWidth;
dimension[1] = window.innerHeight;
Shiny.onInputChange("dimension", dimension);
});
$(window).resize(function(e) {
dimension[0] = window.innerWidth;
dimension[1] = window.innerHeight;
Shiny.onInputChange("dimension", dimension);
});
' 
        )
      ),
      tags$head(
        tags$script(
          # http://stackoverflow.com/questions/33250075/get-screen-resolution-from-javascript-in-r-shiny
'
$(document).on("shiny:connected", function(e) {
var jsWidth = screen.width; jsHeight = screen.height;
Shiny.onInputChange("GetScreenWidth",jsWidth);
Shiny.onInputChange("GetScreenHeight",jsHeight);
});
'
        )
      ),
      if (scalePlotsToVerticalHeight) {
        tags$head(tags$style(
          # http://stackoverflow.com/questions/26782041/scaling-shiny-plots-to-window-height
          paste0(
            ".shiny-plot-output{height:",
            verticalHeightOfPlots,
            " !important;}"
          )
        ))
      },
      tags$head(
        tags$style(
          type = "text/css",
          # http://stackoverflow.com/questions/37470226/blinking-loading-text-in-r-shiny
"#loadmessage {
position: fixed;
top: 93%;// 10;// 50%;
left: 96%;// 50%;
ocacity: 0.50; 
text-align: right;// center;
//font-weight: bold;
font-size: 300%;
color: #000000;
z-index: 105;
animation: blinker 1s linear infinite;
}"
        )
      ),
      modalDialog(title="Welcome",
                  size = "m", # c("m", "s", "l"), 
                  easyClose = TRUE, 
                  fade = TRUE,
                  # uiOutput(outputId = "SplashPageWidget"),
                  wellPanel(
                    style = paste0("background-color: ", AppBackgroundColor, ";",
                                   "border:4px; border-color:#458cc3;"
                    ),
                    helpText("Welcome to PeZdemoR..."),
                    br(),
                    HTML(
                      markdown::markdownToHTML(
                        fragment.only = TRUE,
                        text = paste0("This is a sample web-application made with Shiny to demonstrate some basic DSP concepts."
                        ) # end paste0
                      ) # end markdownToHTML
                    ), # end HTML
                    a(img(
                      src = "favicon.png",
                      height = 36L,
                      width = 36L
                    ), href = "http://www.rstudio.com/shiny"),
                    br(),
                    hr(),
                    HTML('<script type="text/javascript" src="https://www.brainyquote.com/link/quotebr.js"></script><small><i><a href="https://www.brainyquote.com/quotes_of_the_day.html" target="_blank" rel="nofollow">more Quotes</a></i></small>'),
                    hr(),
                    HTML(markdown::markdownToHTML(fragment.only=TRUE,text="&copy; 2016-2017 P. Squires, DiYZeR Research Group, ECE Dept, Univ. of Victoria"))
                  ), # end wellPanel
                  footer = modalButton("Dismiss")
      ),
      conditionalPanel(
        condition = "$('html').hasClass('shiny-busy')",
        tags$div("...",
                 id = "loadmessage"),
        tags$script( 
          # http://stackoverflow.com/questions/37470226/blinking-loading-text-in-r-shiny
          HTML("
(function blink() { 
$('#loadmessage').fadeOut(500).fadeIn(500, blink); 
})();
"
          )
        )
      ),
      # navbarPage ----
      navbarPage(
        id = "mainNavBarPage",
        # selected="MainPlots",
        title = div(HTML(MyAppNameAndTheme),
                    img(src = "bigorb.png", height = 20L
                        , alt = "R-logo big orb"
                    )),
        windowTitle = paste(MyChosenTheme,
                            "PeZ Demo"),
        position = "static-top",
        inverse = FALSE,
        collapsible = TRUE,
        fluid = TRUE,
        # 'Plots' tabPanel ----
        tabPanel(
          style = paste0("background-color: ", NavBarPageBackgroundColor,
                         ";"),
          title = "Plots",
          # id="MainPlots",
          # selected="PeZDemoPlotsPage",
          icon = shiny::icon("bar-chart-o", lib = "font-awesome"), # http://fontawesome.io/icons/
          
          shinyBS::bsModal(
            id = "modalExample",
            title = "Your plot",
            trigger = "pb_showgph",
            size = "large",
            plotOutput("plotshowgph"),
            downloadButton(outputId = "downloadShowgphPlot", label = "Download")
          ),
          fluidPage(title = "PeZDemo Plots Page", 
                    id="PeZDemoPlotsPage",
                    fluidRow(
            # . sidebarLayout ----
            sidebarLayout(
              position = "left",
              fluid = TRUE,
              
              # https://groups.google.com/forum/#!topic/shiny-discuss/iOqdpc-C80Y
              rintrojs::introBox(
              # .. sidebarPanel ----
              sidebarPanel(
                style = paste0("background-color: ",
                               MainSidePanelBackgroundColor, ";"),
                width = mySidebarWidth,
                tags$head(if (!is.null(AppBackgroundColor)) {
                  tags$style(paste0("body {background-color: ", AppBackgroundColor,
                                    "; }"))
                }),
                tags$span(
                  title = "tooltip: Sidebar of the Plots page",
                  fluidRow(
                    column(
                      width = 6L,
                      align = "center",
                      shinyjs::hidden(
                        tags$span(
                          title = "tooltip: add Poles",
                          shinyBS::bsButton(
                            inputId = "pb_pp",
                            label = "2P+",
                            width = "31.5%",
                            style = "primary",
                            size = "small",
                            type = "toggle",
                            block = FALSE,
                            disabled = FALSE,
                            value = FALSE,
                            icon = shiny::icon("times", lib = "font-awesome")
                          )
                        ),
                        tags$span(
                          title = "tooltip: add Zeros",
                          shinyBS::bsButton(
                            inputId = "pb_zz",
                            label = "2Z+",
                            width = "31.5%",
                            style = "primary",
                            size = "small",
                            type = "toggle",
                            block = FALSE,
                            disabled = FALSE,
                            value = TRUE,
                            icon = shiny::icon("circle-o",
                                               lib = "font-awesome")
                          )
                        ),
                        tags$span(
                          title = "tooltip: add both Poles and Zeros (quad)",
                          shinyBS::bsButton(
                            inputId = "pb_pz",
                            label = "2P2ZQ+",
                            width = "31.5%",
                            style = "primary",
                            size = "small",
                            type = "action",
                            block = FALSE,
                            disabled = FALSE,
                            icon = shiny::icon("times-circle-o", lib = "font-awesome")
                          )
                        ),
                        br()
                      ), # end hidden
                      tags$span(
                        title = "tooltip: remove _all_ Poles",
                        shinyBS::bsButton(
                          inputId = "pb_mp",
                          label = "-P*",
                          width = "31.5%",
                          style = "warning",
                          size = "small",
                          type = "action",
                          block = FALSE,
                          disabled = FALSE,
                          icon = shiny::icon("minus-square", lib = "font-awesome")
                        )
                      ),
                      tags$span(
                        title = "tooltip: remove _all_ Zeros",
                        shinyBS::bsButton(
                          inputId = "pb_mz",
                          label = "-Z*",
                          width = "31.5%",
                          style = "warning",
                          size = "small",
                          type = "action",
                          block = FALSE,
                          disabled = FALSE,
                          icon = shiny::icon("minus-circle", lib = "font-awesome")
                        )
                      ),
                      tags$span(
                        title = "tooltip: remove _all_ Poles and Zeros",
                        shinyBS::bsButton(
                          inputId = "pb_ma",
                          label = "-PZ**",
                          width = "31.5%",
                          style = "danger",
                          size = "small",
                          type = "action",
                          block = FALSE,
                          disabled = FALSE,
                          icon = shiny::icon("erase", lib = "glyphicon")
                        )
                      )
                    ),
                    column(
                      width = 6L,
                      align = "center",
                      shinyjs::hidden(
                        tags$span(
                          title = "Poles/ Zeros can be added continuously -- i.e.,continuous mouse-clicks on the pole-zero plot will add appropriate poles/zeros. If not set, one of PP,ZZ,PZ pushbuttons has to be selected, before each mouse-click, to add",
                          shinyBS::bsButton(
                            inputId = "tb_addrepeatedly",
                            label = "Rpt",
                            width = "48%",
                            style = "primary",
                            size = "small",
                            type = "toggle",
                            block = FALSE,
                            disabled = FALSE,
                            value = FALSE,
                            icon = shiny::icon("repeat",
                                               lib = "font-awesome")
                          )
                        )
                      ),
                      tags$span(
                        title = "add poles/ zeros as conjugate-pairs.  If this is set, then poles/ zeros added as conjugate-pairs",
                        shinyBS::bsButton(
                          inputId = "tb_addconjugate",
                          label = "Conj.",
                          width = "48%",
                          style = "primary",
                          size = "small",
                          type = "toggle",
                          block = FALSE,
                          disabled = FALSE,
                          value = TRUE,
                          icon = shiny::icon("compress",
                                             lib = "font-awesome")
                        )
                      ),
                      shinyjs::hidden(
                        tags$span(
                          title = paste(
                            "tooltip: Select/ move already-exiting poles/ zeros using mouse, within Pole-Zero plot.",
                            "Select this option and then choose a pole/zero in the pole-zero plot.",
                            "By holding the pole/zero using the mouse, you can move it to different locations.",
                            "With the movement of poles/zeros, you should be able to see the corresponding variations in system-response.",
                            "... Or: Unlink already-existing pole/zero pairs.",
                            "Use 'Click Enable'/ 'Select Pair' to select a pole/zero pair.",
                            "Then selecting 'Unlink' will unlink the pair, and they will be converted to separate poles/zeros."
                          ),
                          shinyBS::bsButton(
                            inputId = "tb_clickenable",
                            label = "Pair",
                            width = "48%",
                            style = "primary",
                            size = "small",
                            type = "toggle",
                            block = FALSE,
                            disabled = FALSE,
                            value = TRUE,
                            icon = shiny::icon("link", lib = "font-awesome")
                          )
                        ),
                        tags$span(
                          title = paste(
                            "tooltip: Unlink already-existing pole/zero pairs.",
                            "Use 'Click Enable'/ 'Select Pair' to select a pole/zero pair.",
                            "Then selecting 'Unlink' will unlink the pair, and they will be converted to separate poles/zeros."
                          ),
                          shinyBS::bsButton(
                            inputId = "pb_unlink",
                            label = "Unlink",
                            width = "48%",
                            style = "default",
                            size = "small",
                            type = "action",
                            block = FALSE,
                            disabled = FALSE,
                            icon = shiny::icon("chain-broken", lib = "font-awesome")
                          )
                        )
                      )
                    )
                  ),
                  tabsetPanel(
                    type = "tabs",
                    id = "pzPlotsPanel",
                    tabPanel(
                      style = paste0("background-color: ",
                                     TabPanelBackgroundColor, ";"),
                      title = "Rectangular",
                      tags$span(
                        title = "tooltip: Rectangular-Plot Tab of Sidebar of the Plots page",
                        tags$span(
                          title = "tooltip: pole-zero Plot (z-plane) tab\n(use browser's right-mouse-click/ context-menu for image download-options)",
                          shinyjs::hidden(fluidRow(
                            column(
                              6L,
                              tags$span(
                                title = "Lock XY-Coordinates",
                                checkboxGroupInput(
                                  inputId = "DragLockGrp",
                                  label = NULL,
                                  choices = c(`Drag-Lock x/Real` = "real_motionT",
                                              `Drag-Lock y/Imag` = "imag_motion"),
                                  selected = NULL,
                                  inline = TRUE
                                )
                              )
                            ), column(6L, tags$span(
                              title = "Mirror-image point",
                              checkboxGroupInput(
                                inputId = "MirrorImageGrp",
                                label = NULL,
                                choices = c(
                                  `Mirror x-axis` = "mirrorx",
                                  `Mirror 2XO unit-circle` = "mirroruc"
                                ),
                                selected = NULL,
                                inline = TRUE
                              )
                            ))
                          )), # end hidden
                          shinyBS::bsCollapse(
                            id = "sidePanelPlotCollapse",
                            multiple = TRUE,
                            open = "sidePanelPlotCollapse1",
                            shinyBS::bsCollapsePanel(
                              value = "sidePanelPlotCollapse1",
                              title = "PZ-Plot Panel (click to expand/ collapse):",
                              style = "info",
                              wellPanel( # http://www.w3schools.com/bootstrap/bootstrap_wells.asp
                                style = paste0("background-color: ",
                                               WellPanelBackgroundColor, ";"),
                                if (scalePlotsToVerticalHeight) {
                                  tags$head(tags$style(
                                    paste0(
                                      "#axes_pzplot{height:",
                                      verticalHeightOfPlots,
                                      " !important;}"
                                    )
                                  ))
                                },
                                div(
                                  style = "position:relative",
                                  plotOutput(
                                    outputId = "axes_pzplot",
                                    width = "100%",
                                    height = "650px",
                                    inline = FALSE,
                                    click = clickOpts(id = "pzplot_click",
                                                      clip = TRUE),
                                    dblclick = dblclickOpts(id = "pzplot_dblclick",
                                                            clip = TRUE, delay = 400),
                                    brush = brushOpts(
                                      id = "pzplot_brush",
                                      fill = "#9cf",
                                      stroke = "#036",
                                      opacity = 0.25,
                                      delay = 300,
                                      delayType = "throttle",
                                      clip = TRUE,
                                      direction = "xy",
                                      resetOnNew = TRUE
                                    ),
                                    hover = hoverOpts(
                                      id = "pzplot_hover",
                                      delay = 200,
                                      delayType = "throttle"
                                    )
                                  ),
                                  uiOutput("hover_info")
                                )
                              ),
                              tags$span(
                                title = "tooltip: 'Zoom in' within Pole-Zero plot, to better identify precise location of poles and zeros",
                                shinyBS::bsButton(
                                  inputId = "tb_zoomenable",
                                  label = "Zoom Enable/ Disable",
                                  style = "primary",
                                  size = "small",
                                  type = "toggle",
                                  block = TRUE,
                                  disabled = FALSE,
                                  value = FALSE,
                                  icon = shiny::icon("search",
                                                     lib = "font-awesome")
                                ),
                                conditionalPanel(
                                  condition = "input.tb_zoomenable",
                                  tags$span(
                                    title = "tooltip: The chosen-range, [min, max], can be dragged together, as one piece",
                                    sliderInput(
                                      inputId = "zoomlimX",
                                      label = HTML("max X,Y-Limits   (-)&RightTee;&LeftTee;(+)"), # https://dev.w3.org/html5/html-author/charref
                                      min = -5L,
                                      max = 5L,
                                      value = c(-1L,
                                                1L),
                                      step = 0.25,
                                      ticks = TRUE,
                                      animate = FALSE,
                                      sep = " ",
                                      dragRange = TRUE
                                    ),
                                    sliderInput(
                                      inputId = "zoomlimY",
                                      label = NULL,
                                      min = -5L,
                                      max = 5L,
                                      value = c(-1L,
                                                1L),
                                      step = 0.25,
                                      ticks = TRUE,
                                      animate = FALSE,
                                      sep = " ",
                                      post = "j",
                                      dragRange = TRUE
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    tabPanel(
                      style = paste0("background-color: ",
                                     TabPanelBackgroundColor, ";"),
                      title = "Polar",
                      tags$span(
                        title = "tooltip: Polar-Plot Tab of Sidebar of the Plots page",
                        tags$span(
                          title = "tooltip: pole-zero Plot tab, polar\n(use browser's right-mouse-click/ context-menu for image download-options)",
                          shinyjs::hidden(
                            tags$span(
                              title = "tooltip: Lock Polar-Coordinates",
                              checkboxGroupInput(
                                inputId = "LockCoordsPolarGrp",
                                label = NULL,
                                choices = c(
                                  `Lock r (circ. motion)` = "circular_motionT",
                                  `Lock theta (rad. motion)` = "radial_motionT",
                                  `Mirror x-axis` = "mirrorx",
                                  `Mirror y-axis` = "mirrory",
                                  `Mirror unit-circle` = "mirroruc"
                                ),
                                selected = NULL,
                                inline = TRUE
                              )
                            )
                          ),
                          wellPanel(
                            style = paste0("background-color: ",
                                           WellPanelBackgroundColor, ";"),
                            if (scalePlotsToVerticalHeight) {
                              tags$head(tags$style(
                                paste0(
                                  "#axes_pzplotPolar{height:",
                                  verticalHeightOfPlots,
                                  " !important;}"
                                )
                              ))
                            },
                            plotOutput(
                              outputId = "axes_pzplotPolar",
                              width = "100%",
                              height = "650px",
                              inline = FALSE
                            )
                          )
                        )
                      )
                    ),
                    shinyjs::hidden(tabPanel(
                      style = paste0("background-color: ",
                                     TabPanelBackgroundColor, ";"),
                      title = "3js",
                      tags$span(
                        title = "tooltip: 3D-Plot Tab of Sidebar of the Plots page",
                        tags$span(
                          title = "tooltip: pole-zero Plot tab, 3D Interactive\n(shift-Mousewheel to zoom)\n(use browser's right-mouse-click/ context-menu for static-image download-options)",
                          tags$span(
                            title = "tooltip: 3D-Graph Parameters",
                            fluidRow(
                              tags$span(title = "tooltip: suggested number-of-ticks, per axis",
                                        column(
                                          width = 4L,
                                          numericInput(
                                            inputId = "nticks3D",
                                            label = "N ticks",
                                            value = 6L,
                                            min = 1L,
                                            max = 20L,
                                            step = 1L
                                          )
                                        )),
                              tags$span(title = "tooltip: either a single hex or named color name (all points same color), or a vector of #' hex or named color names as long as the number of data points to plot.",
                                        column(
                                          width = 4L,
                                          numericInput(
                                            inputId = "colors3D",
                                            label = "N colors",
                                            value = 32L,
                                            min = 1L,
                                            max = 64L,
                                            step = 1L
                                          )
                                        )),
                              conditionalPanel(
                                condition = "(input.renderer3D == 'canvas') || (input.renderer3D == 'auto')",
                                tags$span(title = "tooltip: (only supported by the 'canvas'-type renderer)",
                                          column(
                                            width = 4L,
                                            numericInput(
                                              inputId = "sizes3D",
                                              label = "Point-size",
                                              value = 0.1,
                                              min = 0.1,
                                              max = 6L,
                                              step = 0.1
                                            )
                                          ))
                              )
                            ),
                            fluidRow(
                              tags$span(title = "tooltip: The 'canvas' renderer is the fallback rendering option when 'webgl' is not available. Select 'auto' to automatically choose between the two. The two renderers produce slightly different-looking output and have different available options (see above). Use the 'webgl' renderer for plotting large numbers of points (if available). Use the 'canvas' renderer to excercise finer-control of plotting of smaller numbers of points.",
                                        column(
                                          width = 4L,
                                          selectInput(
                                            "renderer3D",
                                            label = "Render method",
                                            choices = list(
                                              Auto = "auto",
                                              Canvas = "canvas",
                                              WebGL = "webgl"
                                            ),
                                            selected = "webgl"
                                          )
                                        )),
                              tags$span(title = "tooltip: display of a grid",
                                        column(
                                          width = 4L, checkboxInput("usegrid3D",
                                                                   label = "Grid", value = FALSE)
                                        )),
                              tags$span(title = "tooltip: display of a plane at z=0\nBetter, but slower, in 'canvas' mode",
                                        column(
                                          width = 4L,
                                          checkboxInput("usezerozplane",
                                                        label = "plane at z=0", value = FALSE)
                                        ))
                            )
                          ),
                          wellPanel(
                            style = paste0("background-color: ",
                                           WellPanelBackgroundColor, ";"),
                            if ((scalePlotsToVerticalHeight)) {
                              tags$head(tags$style(
                                paste0(
                                  "#axes_pzplot3D{height:",
                                  verticalHeightOfPlots,
                                  " !important;}"
                                )
                              ))
                            },
                            threejs::scatterplotThreeOutput(
                              outputId = "axes_pzplot3D",
                              width = "100%",
                              height = "650px"
                            )
                          )
                        )
                      )
                    )), # end hidden
                    tabPanel(
                      style = paste0("background-color: ",
                                     TabPanelBackgroundColor, ";"),
                      title = "3D Interactive (OpenGL)",
                      tags$span(
                        title = "tooltip: RGL-Plot Tab of Sidebar of the Plots page",
                        tags$span(
                          title = "tooltip: pole-zero Plot tab, RGL Interactive (drag to rotate)\n(shift-Mousewheel to zoom)\n(use browser's right-mouse-click/ context-menu for static-image download-options)",
                          if ((scalePlotsToVerticalHeight)) {
                            tags$head(tags$style(
                              paste0(
                                "#axes_pzplotRGL{height:",
                                verticalHeightOfPlots,
                                " !important;}"
                              )
                            ))
                          },
                          rgl::rglwidgetOutput(outputId = "axes_pzplotRGL",
                                               width = "auto")
                        )
                      ),
                      br(),
                      downloadButton(outputId = "downloadRGL",
                                     label = "Save RGL interactive-plot as WebGL .html file"),
                      tags$hr()
                    )
                  ),
                  conditionalPanel(
                    condition = "input.checkboxRAY",
                    wellPanel(
                      style = paste("background-color:", WellPanelBackgroundColor,
                                    ";"),
                      uiOutput(outputId = "slider1Widget"),
                      uiOutput(outputId = "stretchyslider1rangeWidget"),
                      shinyjs::hidden(
                        uiOutput(outputId = "stretchyslider1stepWidget"),
                        uiOutput(outputId = "slider1animintervalWidget")
                      )
                    )
                  ),
                  hr(),
                  fluidPage(
                    title = "tooltip: fluidPage2",
                    tabsetPanel(
                      type = "tabs",
                      id = "tabPoleZeroEditing",
                      tabPanel(
                        style = paste0("background-color: ",
                                       SubTabPanelBackgroundColor, ";"),
                        title = "Real+Imag",
                        value = "RealImag",
                        align = "center",
                        tags$span(title = "tooltip: Rectangular-Coordinates Tab of Sidebar",
                                  fluidRow(
                                    column(
                                      width = 6L,
                                      title = "enter numbers/ R-language equations, or pull-down for previous entries",
                                      tags$span(
                                        title = "tooltip: enter numbers/ R-language equations, or pull-down for previous entries",
                                        selectizeInput(
                                          inputId = "edit_polezeroloc",
                                          label = "(real)",
                                          choices = if (file.exists("initialpolezerolocs.txt")) {
                                            readLines("initialpolezerolocs.txt")
                                          } else {
                                            c(
                                              0L,
                                              "(1-eps)*exp(-2i*pi/3)",
                                              "(1-8*eps)*exp(-2i*pi/3)",
                                              0.5,
                                              "1-eps",
                                              "1/sqrt(2)",
                                              "1L-0.5*3.276i",
                                              "0.2*j+0.4",
                                              "0.475+sqrt(3)/2*0.95*1i",
                                              "rnorm(1L,mean=0,sd=0.5)",
                                              "rnorm(1L,mean=0,sd=0.5)+rnorm(1L,mean=0,sd=0.5)*1i",
                                              "runif(1L,min=-0.999,max=0.999)",
                                              "cos(pi/3)+1i*sin(pi/3)",
                                              "cospi(0.5)",
                                              "sqrt(3)/2",
                                              "1-2*eps",
                                              "1-8*eps",
                                              Inf,
                                              Infi
                                            )
                                          },
                                          selected = "0",
                                          options = list(create = TRUE,
                                                         placeholder = "type here, or pull-down for options")
                                        )
                                      )
                                    ),
                                    column(
                                      width = 6L,
                                      title = "enter numbers/ R-language equations, or pull-down for previous entries",
                                      tags$span(
                                        title = "tooltip: enter numbers/ R-language equations, or pull-down for previous entries",
                                        selectizeInput(
                                          inputId = "edit_polezerolocImag",
                                          label = HTML("&pm;(imag.) * &jmath;"),
                                          choices = if (file.exists("initialpolezerolocsImag.txt")) {
                                            readLines("initialpolezerolocsImag.txt")
                                          } else {
                                            c(
                                              0L,
                                              0.5,
                                              "1/sqrt(2)",
                                              "cos(pi/3)",
                                              "sinpi(0.5)",
                                              "1-eps",
                                              "rnorm(1L,mean=0,sd=0.5)",
                                              "runif(1L,min=-0.999,max=0.999)",
                                              "-(1-eps)",
                                              "1-2*eps",
                                              "-(1-2*eps)",
                                              "1-8*eps",
                                              "-(1-8*eps)"
                                            )
                                          },
                                          selected = "0",
                                          options = list(create = TRUE,
                                                         placeholder = "type here, or pull-down for options")
                                        )
                                      )
                                    )
                                  ),
                                  fluidRow(
                                    tags$span(
                                      title = "tooltip: equivalent Polar-coordinates",
                                      align = "center",
                                      verbatimTextOutput(outputId = "rect2polarPZplot")
                                    )
                                  ))
                      ),
                      tabPanel(
                        style = paste0("background-color: ", SubTabPanelBackgroundColor,
                                       ";"),
                        title = HTML("r,&theta;"),
                        value = "rtheta",
                        align = "center",
                        tags$span(title = "tooltip: Polar-Coordinates Tab of Sidebar",
                                  fluidRow(
                                    column(
                                      width = 6L,
                                      tags$span(
                                        title = "tooltip: enter numbers/ R-language equations, or pull-down for previous entries",
                                        selectizeInput(
                                          inputId = "edit_polezerolocRadius",
                                          label = HTML("<b>&rarrbfs;</b> (radius)"),
                                          choices = c(
                                            0L,
                                            0.4,
                                            "1-eps",
                                            "1/sqrt(2)",
                                            "sqrt(3)/2",
                                            "abs(rnorm(1L,mean=0,sd=0.5))",
                                            "runif(1L,min=0,max=0.999)",
                                            0.95,
                                            "1-2*eps",
                                            "1-8*eps",
                                            0.5,
                                            0.8,
                                            0.9,
                                            Inf,
                                            Infi
                                          ),
                                          selected = "0",
                                          options = list(create = TRUE, placeholder = "type here, or pull-down for options")
                                        )
                                      )
                                    ),
                                    column(
                                      width = 6L,
                                      title = "enter numbers/ R-language equations, or pull-down for previous entries",
                                      tags$span(
                                        title = "tooltip: enter numbers/ R-language equations, or pull-down for previous entries",
                                        selectizeInput(
                                          inputId = "edit_polezerolocAngle",
                                          label = HTML("<b>&angmsdaa;</b> (angle, rads)"),
                                          choices = c(
                                            "0",
                                            "pi",
                                            "0.25*pi",
                                            "37.2*pi/180",
                                            "runif(1L,min=0,max=1)*2*pi",
                                            "pi/2",
                                            "3*pi/2",
                                            "pi/3",
                                            "2*pi/3",
                                            "pi/4",
                                            "3*pi/4",
                                            "pi/5",
                                            "pi/6",
                                            "pi/7",
                                            "pi/8",
                                            "-pi",
                                            "-0.25*pi",
                                            "-pi/2",
                                            "-pi/3",
                                            "-2*pi/3",
                                            "-pi/4",
                                            "-3*pi/4",
                                            "-pi/5",
                                            "-pi/6",
                                            "-pi/7",
                                            "-pi/8"
                                          ),
                                          selected = "0",
                                          options = list(create = TRUE, placeholder = "type here, or pull-down for options")
                                        )
                                      )
                                    )
                                  ),
                                  fluidRow(
                                    tags$span(
                                      title = "tooltip: equivalent Cartesian (Rectangular)-coordinates",
                                      align = "center",
                                      verbatimTextOutput(outputId = "polar2rectPZplot")
                                    )
                                  ))
                      ),
                      tabPanel(
                        style = paste0("background-color: ", SubTabPanelBackgroundColor,
                                       ";"),
                        title = HTML("Filter &ccupssm;"),
                        value = "CommonFilters",
                        align = "center",
                        tags$span(
                          title = "tooltip: Common-Filters Tab of Sidebar",
                          selectizeInput(
                            inputId = "commonFilters",
                            label = "Common Filters/ Windows",
                            choices = c(
                              `FIR Window, order 40, LPF, 0.3, Hamming window` = "N=40;fir1(n=N,w=0.3,type=\"low\",window=hamming(N+1),scale=TRUE)",
                              `FIR Window, order 10, BPF, 0.3 to 0.65, Hamming window` = "N=10;fir1(n=N,w=c(0.3,0.65),type=\"pass\",window=hamming(N+1),scale=TRUE)",
                              `FIR Window, order 10, BSF, 0.3 to 0.65, Hamming window` = "N=10;fir1(n=N,w=c(0.3,0.65),type=\"stop\",window=hamming(N+1),scale=TRUE)",
                              `FIR, arbitrary piecewise-linear (type II), order 100, BPF, 0.3 to 0.65, Hamming` = "fir2(n=100, f=c(0, 0.3, 0.3, 0.65, 0.65, 1), m=c(0, 0, 1, 0.5, 0, 0), grid_n=512, ramp_n=5, window=hamming(101))",
                              `Arbitrary MA FIR, given b` = "Ma(b=c(1/3,2/3,1/3))",
                              `Arbitrary ARMA IIR, given b,a` = "Arma(b=c(1/3,2/3,1/3), a=c(1,1-eps))",
                              `Arbitrary ARMA IIR, given poles,zeros` = "Zpg(zero=c(-1,-1), pole=c(-(1-eps)), gain=1/3)",
                              `FIR, Zero located at infinity, given b` = "Ma(b=c(1,-fpmaxx))",
                              `Over-damped: real Poles` = "Zpg(zero=c(0), pole=c(-0.92345,0.92345), gain=1)",
                              `Under-damped (damped-sinusoid, exponential envelope): complex-conjugate Poles` = "Zpg(zero=c(0), pole=c(-0.5+0.52345i,-0.5-0.52345i), gain=1)",
                              `Un-damped (oscillating, natural-frequency, resonates): imaginary Poles` = "Zpg(zero=c(0), pole=c(0.92345i,-0.92345i), gain=1)",
                              `Critical-damped: multiple/ repeated/ co-located real Poles` = "Zpg(zero=c(0), pole=c(-0.92345,-0.92345), gain=1)",
                              `LPF, Bilinear z-Transform, conversion of analog-prototype, cutoff=0.4` = "omegac=0.4;omegacprime=tan(omegac*pi/2);Zpg(zero=c(-1), pole=c(-(omegacprime-1)/(omegacprime+1)), gain=omegacprime/(omegacprime+1))",
                              `LPF, Bilinear z-Transform (function), conversion of analog, cutoff=0.4` = "omegac=0.4;signal::bilinear(Sz=c(-fpmaxx/10),Sp=c(-tan(pi*omegac/2)),Sg=tan(pi*omegac/2)/(fpmaxx/10),T=2)",
                              `Transform band-edges of s-plane LPF 0.3 to a z-plane BPF 0.3-0.65` = "omegac1=0.3;omegac2=0.65;signal::sftrans(Sz=c(-1e16),Sp=c(-tan(pi*omegac1/2)),Sg=tan(pi*omegac1/2)/(1e16),W=c(omegac1,omegac2),stop=FALSE)",
                              `Chebyshev I, order 5, 3dB ripple, LPF, 0.3` = "cheby1(n=5,Rp=3,W=0.3,type=\"low\")",
                              `Chebyshev I, order 5, 3dB ripple, HPF, 0.65` = "cheby1(n=5,Rp=3,W=0.65,type=\"high\")",
                              `Chebyshev I, order 5, 3dB ripple, BPF, 0.3 to 0.65` = "cheby1(n=5,Rp=3,W=c(0.3,0.65),type=\"pass\")",
                              `Chebyshev I, order 5, 3dB ripple, BSF, 0.3 to 0.65` = "cheby1(n=5,Rp=3,W=c(0.3,0.65),type=\"stop\")",
                              `Sinusoidal/ Oscillator/ Resonator, 2 poles on imag-axis, near unit-circle, frq=0.5` = "Arma(b=c(1), a=c(1,0,(1-eps)))",
                              `Sinusoidal/ Oscillator/ Resonator, 2 conjugate-poles, frq=ray-slider` = "theta=input$slider1;Zpg(zero=c(0), pole=0.99999999999999*c(exp(theta*pi*1i),exp(-theta*pi*1i)), gain=1)",
                              `L-point Moving-Average FIR, L=5` = "L=5;FftFilter(rep(1/L,times=L),n=512)$b",
                              `Delay-Line (three-terms) 'IIR-equivalent' CIC Mov-Avg filter, N=5` = "N=5;Arma(b=c(1/N,rep(0,times=N-1),-1/N),a=c(1,-(1-eps)))",
                              `Echo/Slapback-effects (delay-line comb), N=450 Samples (at Fs; needs ~10-50msecs)` = "N=450;Arma(b=c(-1/N,rep(0,times=N-1),1/N),a=c(1))",
                              `Cascaded Integrator-Comb CIC (MovAvg FIR), delay R=5 (b,a)` = "R=5;M=1;Arma(b=c(1,rep(0,times=R*M-1),-1), a=c(1,-(1-eps)))",
                              `Cascaded Integrator-Comb CIC (MovAvg FIR), delay R=8 (Zpg)` = "N=8;Zpg(zero=c(1,-1,1i,-1i,1/sqrt(2)+1/sqrt(2)*1i,1/sqrt(2)-1/sqrt(2)*1i,-1/sqrt(2)+1/sqrt(2)*1i,-1/sqrt(2)-1/sqrt(2)*1i), pole=c(1-eps), gain=1/N)",
                              `IIR Comb-Filter, delay-line, 5 poles w/3 zeros` = "Arma(b=c(1,0,0, 0.5^3), a=c(1,0,0,0,0, 0.9^5))",
                              `Feedback Comb-filter (peaks), delay-line, poles only, delay=8, positive alpha`="K=8;alpha=0.9;Arma(b=c(1), a=c(1,rep(0,times=K-1),alpha))",
                              `Feed-forward Comb-filter (humps), delay-line, zeros only, delay=8, negative alpha`="K=8;alpha=-1;Arma(b=c(1,rep(0,times=K-1),alpha), a=c(1))",
                              `unicomb, Universal Comb, delay-line (Zol02), FIR, tines-down, FFwd=r.v.`="delayM=8;FB=0;FFwd=0.7+runif(1,min=0,max=0.3);BL=runif(1,min=0.1,max=0.9);Arma(b=c(BL,rep(0,times=delayM-1),BL*FFwd),a=c(1))",
                              `unicomb, Universal Comb, delay-line (Zol02), IIR, tines-up, FB=r.v.`="delayM=8;FB=0.7+runif(1,min=0,max=0.3);FFwd=0;BL=1;Arma(b=c(BL),a=c(1,rep(0,times=delayM-1),-BL*FB))",
                              `unicomb, Universal Comb, delay-line (Zol02), allpass, FB=0.4`="delayM=8;FB=0.4;FFwd=(1-eps);BL=-FB;Arma(b=c(BL,rep(0,times=delayM-1),FFwd),a=c(1,rep(0,times=delayM-1),-FB))",
                              `unicomb, Universal Comb, delay-line (Zol02), delay only, BL=0`="delayM=8;BL=1e-10;FB=0;FFwd=(1-eps);Arma(b=c(BL,rep(0,times=delayM-1),FFwd),a=c(1))",
                              `Integrator/ Accumulator (LPF), 1/s, given b,a; pole at +1, zero at -1` = "Arma(b=c(1,1), a=c(1,-(1-eps)))",
                              `Differentiator/ Slope (HPF), pole at -1, zero at +1`= "Zpg(zero=c(1), pole=c(-(1-eps)), gain=1)",
                              `Notch-filter comb, Fractional-Sample Delay-line, D=2pi/omega0 (Pei Tseng '98 Fig 2)` = "omega0=2/9*pi;D=2*pi/omega0;rho=0.99;Arma(b=c(1,rep(0,times=floor(D-1)),-1), a=c(1,rep(0,times=floor(D-1)),-(rho)^D))",
                              `Peaking-filter/ Resonance, fc=ray-slider` = "theta=input$slider1;rp=0.999;rz=0.997;Zpg(zero=c(rz*exp(theta*pi*1i),rz*exp(-theta*pi*1i)), pole=c(rp*exp(theta*pi*1i),rp*exp(-theta*pi*1i)), gain=1)",
                              `Notch-Out Filter, fc=ray-slider` = "theta=input$slider1;rp=0.997;rz=0.999;Zpg(zero=c(rz*exp(theta*pi*1i),rz*exp(-theta*pi*1i)), pole=c(rp*exp(theta*pi*1i),rp*exp(-theta*pi*1i)), gain=1)",
                              `Freq-Sampling IIR algorithm (unstable), ord. 12, fc=0.65, delay=7 (WSL)` = "N=12;D=7;fc=0.65;L=2*N;FF=matrix(data=0,nrow=N,ncol=1);for (k in seq(1,N,by=1)){f=2*k/(L+1);if (f <= fc) {FF[k]=exp(-1i*D*f*pi)}};Fb=Conj(FF);FF=c(1,FF,pracma::flipud(Fb));h=Re(pracma::ifft(FF));r1=t(h);r=c(r1[1],pracma::fliplr(as.matrix(t(r1[2:length(r1)]))));H=pracma::Toeplitz(h,r);H0=H[,1:(N+1)];H1=H0[1:(N+1),];h1=H0[seq((N+2),(L+1)),1];H2=H0[seq((N+2),(L+1)),seq(2,(N+1))];ah=-pracma::inv(H2) %*% h1;a=c(1,ah);b=H1 %*% a;Arma(a=a,b=b)",
                              `Least-Squares IIR algorithm (unstable), ord. 12, fc=0.65, delay=7, 120 Samples (WSL)` = "N=12;D=7;L=120;fc=0.65;L1=0.5*L;FF=matrix(data=0,nrow=L1,ncol=1);for (k in seq(1,L1)){f=2*k/(L+1);if (f <= fc) {FF[k]=exp(-1i*D*f*pi)}};Fb=Conj(FF);FF=c(1,FF,pracma::flipud(Fb));h=Re(pracma::ifft(FF));r1=t(h);r=c(r1[1],pracma::fliplr(as.matrix(t(r1[2:length(r1)]))));H=pracma::Toeplitz(h,r);H0=H[,1:(N+1)];H1=H0[1:(N+1),];h1=H0[seq((N+2),(L+1)),1];H2=H0[seq((N+2),(L+1)),seq(2,(N+1))];ah=-pracma::inv(t(H2) %*% H2) %*% t(H2) %*% h1;a=c(1,ah);b=H1 %*% a;Arma(a=a,b=b)",
                              `All-Pass, poles within circle, zeros outside at conjugate-reciprocal, random-angle` = "r1=1.3;theta=runif(1,min=-1,max=1);Zpg(zero=c(r1*exp(theta*pi*1i),r1*exp(-theta*pi*1i)), pole=c((1/r1)*exp(theta*pi*1i),(1/r1)*exp(-theta*pi*1i)), gain=1)",
                              `All-Pass, pole at 0, zero at infinity` = "Arma(b=c(1,-fpmaxx), a=c(1))",
                              `All-Pass, reversed-ordering of (real) filter-coefficients` = "myb=c(1,2,3,4,5,6);Arma(b=myb, a=rev(myb))",
                              `Min. Phase, GrpDelay < 2 (Oppenheim Schafer Buck, 1989, Fig 5.30a)` = "Zpg(zero=c(0.9*exp(0.6*pi*1i),0.9*exp(-0.6*pi*1i),0.8*exp(0.8*pi*1i),0.8*exp(-0.8*pi*1i)), pole=c(0), gain=1)",
                              `Max. Phase, GrpDelay < 12 (Oppenheim Schafer Buck, 1989, Fig 5.30b)` = "Zpg(zero=c(1/0.9*exp(0.6*pi*1i),1/0.9*exp(-0.6*pi*1i),1/0.8*exp(0.8*pi*1i),1/0.8*exp(-0.8*pi*1i)), pole=c(0), gain=1)",
                              `Hilbert allpass (absolute values), order 40, Hamming window (WSL)` = "N=41;M=20;hz=matrix(data=0,nrow=1,ncol=M);zw=seq(1,(M-1),by=2);hz[seq(1,(M-1),by=2)]=2/ (pi*zw);hd=c(-pracma::fliplr(as.matrix(hz)),0,hz);w=signal::hamming(N);hd*w",
                              `Ideal Differentiator (HPF) (noiseless-inputs only; else, corruption), 23pt Hamming,fs=512,fc=0.3 (WSL)` = "t=seq(0,2-1/512,by=1/512);fs=512;Ts=1/fs;N=23;M=(N-1)/2;n=1:M;h=cos(n*pi)/(Ts*n);h=c(-pracma::fliplr(as.matrix(t(h))),0,h);win=signal::hamming(N);win*h",
                              `Differentiator, Band-Limited, 23-pt Hamming window, fs=512, fc=0.3 (WSL)` = "fs=512;Ts=1/fs;N1=23;M=(N1-1)/2;n=0:(M-1);k=M-n;k2=k^2;fc=0.3*pi;h1=sin(k*fc);h2=(fc*k)*cos(k*fc);hd=(h1-h2)/(Ts*pi*k2);hd=c(hd,0,-pracma::fliplr(as.matrix(t(hd))));win=signal::hamming(N1);win*hd",
                              `Butterworth, order 5, LPF, 0.3` = "butter(n=5,W=0.3,type=\"low\")",
                              `Butterworth, order 5, HPF, 0.65` = "butter(n=5,W=0.65,type=\"high\")",
                              `Butterworth, order 5, BPF, 0.3 to 0.65` = "butter(n=5,W=c(0.3,0.65),type=\"pass\")",
                              `Butterworth, order 5, BSF, 0.3 to 0.65` = "butter(n=5,W=c(0.3,0.65),type=\"stop\")",
                              `Chebyshev II, order 5, 3 dB ripple, LPF, 0.3` = "cheby2(n=5,Rp=20,W=0.3,type=\"low\")",
                              `Chebyshev II, order 5, 3 dB ripple, HPF, 0.65` = "cheby2(n=5,Rp=20,W=0.65,type=\"high\")",
                              `Chebyshev II, order 5, 3 dB ripple, BPF, 0.3 to 0.65` = "cheby2(n=5,Rp=20,W=c(0.3,0.65),type=\"pass\")",
                              `Chebyshev II, order 5, 3 dB ripple, BSF, 0.3 to 0.65` = "cheby2(n=5,Rp=20,W=c(0.3,0.65),type=\"stop\")",
                              `Elliptical, order 5, ripple: 3dB, (40dB stopband), LPF, 0.3` = "ellip(n=5,Rp=3,Rs=40,W=0.3,type=\"low\")",
                              `Elliptical, order 5, ripple: 3dB, (40dB stopband), HPF, 0.65` = "ellip(n=5,Rp=3,Rs=40,W=0.65,type=\"high\")",
                              `Elliptical, order 5, ripple: 3dB, (40dB stopband), BPF, 0.3 to 0.65` = "ellip(n=5,Rp=3,Rs=40,W=c(0.3,0.65),type=\"pass\")",
                              `Elliptical, order 5, ripple: 3dB, (40dB stopband), BSF, 0.3 to 0.65` = "ellip(n=5,Rp=3,Rs=40,W=c(0.3,0.65),type=\"stop\")",
                              `Min. Order, Butterworth: ripple: 0.5dB (29dB stopband), LPF, 0.3` = "butter(n=buttord(Wp=0.285, Ws=0.345, Rp=0.5, Rs=29))",
                              `Min. Order, Chebyshev I: ripple: 0.5dB (29dB stopband), LPF, 0.3` = "cheby1(n=cheb1ord(Wp=0.3, Ws=0.34, Rp=0.5, Rs=29))",
                              `Min. Order, Elliptical: ripple: 0.5dB (29dB stopband), LPF, 0.3` = "ellip(n=ellipord(Wp=0.3, Ws=0.34, Rp=0.5, Rs=29))",
                              `LPF, Bilinear z-Transform, given b,a, cutoff=0.3, Dodge/ Jerse 1985 (Zolz2003)`="omegac=0.3;zeta=0.7;C=1/(tan(pi*omegac/2));b0=1/(1+2*zeta*C+C^2);Arma(b=c(b0,2*b0,b0),a=c(1,2*b0*(1-C^2),b0*(1-2*zeta*C+C^2)))",
                              `Windowed Fourier FIR, length=30, LPF, 0.6, Hamming (WSL)`="L=30;N=L-1;f_type=1;f_para=0.6;w_type=2;M=(N-1)/2;n=0:(M-1);if (f_type==0) {omec1=0;omec2=pi;hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='all-pass';fparamstr='[0,+infty]'} else if (f_type==1) {omec=pi*f_para;hd=sin((n-M)*omec)/((n-M)*pi);hd=c(hd,omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='LPF';fparamstr=paste0('[0, ',toString(f_para[1]),'pi]')} else if (f_type==2) {omec=pi*f_para;hd=-sin((n-M)*omec)/((n-M)*pi);hd=c(hd,1-omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='HPF';fparamstr=paste0('[',toString(f_para[1]),'pi, +infty]')} else if (f_type==3) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BPF';fparamstr=paste0('[',toString(f_para[1]),'pi, ',toString(f_para[2]),'pi]')} else if (f_type==4) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec1)-sin((n-M)*omec2))/((n-M)*pi);hd=c(hd,1-(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BSF';fparamstr=paste0('[',toString(f_para[1]),'pi,',toString(f_para[2]),'pi]')};if (w_type==0) {w=matrix(data=1,nrow=1,ncol=N);w_type_name='(rect)'} else if (w_type==1) {w=0.5*(1-cos(pi*n/M));w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hann'} else if (w_type == 2) {w=0.54-0.46*cos(pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hamm'} else if (w_type==3) {w=0.42-0.5*cos(pi*n/M)+0.08*cos(2*pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Blckmn'} else if (w_type==4) {if (is.na(b)) {b=5};w=besselI(nu=0, x=b*sqrt(1-(n/M-1)^2))/besselI(nu=0,x=b);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name=paste0('Kaiser,beta=',toString(b))};Ma(b=w*hd)",
                              # `Windowed Fourier FIR, length=8, HPF, 0.5, Blackman (WSL)`="N=7;f_type=2;f_para=0.5;w_type=3;M=(N-1)/2;n=0:(M-1);if (f_type==0) {omec1=0;omec2=pi;hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='all-pass';fparamstr='[0,+infty]'} else if (f_type==1) {omec=pi*f_para;hd=sin((n-M)*omec)/((n-M)*pi);hd=c(hd,omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='LPF';fparamstr=paste0('[0, ',toString(f_para[1]),'pi]')} else if (f_type==2) {omec=pi*f_para;hd=-sin((n-M)*omec)/((n-M)*pi);hd=c(hd,1-omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='HPF';fparamstr=paste0('[',toString(f_para[1]),'pi, +infty]')} else if (f_type==3) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BPF';fparamstr=paste0('[',toString(f_para[1]),'pi, ',toString(f_para[2]),'pi]')} else if (f_type==4) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec1)-sin((n-M)*omec2))/((n-M)*pi);hd=c(hd,1-(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BSF';fparamstr=paste0('[',toString(f_para[1]),'pi,',toString(f_para[2]),'pi]')};if (w_type==0) {w=matrix(data=1,nrow=1,ncol=N);w_type_name='(rect)'} else if (w_type==1) {w=0.5*(1-cos(pi*n/M));w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hann'} else if (w_type == 2) {w=0.54-0.46*cos(pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hamm'} else if (w_type==3) {w=0.42-0.5*cos(pi*n/M)+0.08*cos(2*pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Blckmn'} else if (w_type==4) {if (is.na(b)) {b=5};w=besselI(nu=0, x=b*sqrt(1-(n/M-1)^2))/besselI(nu=0,x=b);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name=paste0('Kaiser,beta=',toString(b))};Ma(b=w*hd)",
                              `Windowed Fourier FIR, length=70, BPF, 0.35-0.65, Kaiser, beta=5 (WSL)`="L=70;N=L-1;f_type=3;f_para=c(0.35,0.65);w_type=4;b=5;M=(N-1)/2;n=0:(M-1);if (f_type==0) {omec1=0;omec2=pi;hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='all-pass';fparamstr='[0,+infty]'} else if (f_type==1) {omec=pi*f_para;hd=sin((n-M)*omec)/((n-M)*pi);hd=c(hd,omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='LPF';fparamstr=paste0('[0, ',toString(f_para[1]),'pi]')} else if (f_type==2) {omec=pi*f_para;hd=-sin((n-M)*omec)/((n-M)*pi);hd=c(hd,1-omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='HPF';fparamstr=paste0('[',toString(f_para[1]),'pi, +infty]')} else if (f_type==3) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BPF';fparamstr=paste0('[',toString(f_para[1]),'pi, ',toString(f_para[2]),'pi]')} else if (f_type==4) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec1)-sin((n-M)*omec2))/((n-M)*pi);hd=c(hd,1-(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BSF';fparamstr=paste0('[',toString(f_para[1]),'pi,',toString(f_para[2]),'pi]')};if (w_type==0) {w=matrix(data=1,nrow=1,ncol=N);w_type_name='(rect)'} else if (w_type==1) {w=0.5*(1-cos(pi*n/M));w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hann'} else if (w_type == 2) {w=0.54-0.46*cos(pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hamm'} else if (w_type==3) {w=0.42-0.5*cos(pi*n/M)+0.08*cos(2*pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Blckmn'} else if (w_type==4) {if (is.na(b)) {b=5};w=besselI(nu=0, x=b*sqrt(1-(n/M-1)^2))/besselI(nu=0,x=b);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name=paste0('Kaiser,beta=',toString(b))};Ma(b=w*hd)",
                              `Windowed Fourier FIR, length=72, BSF, 0.30-0.70, Kaiser, beta=5 (WSL)`="L=72;N=L-1;f_type=4;f_para=c(0.3,0.7);w_type=4;b=5;M=(N-1)/2;n=0:(M-1);if (f_type==0) {omec1=0;omec2=pi;hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='all-pass';fparamstr='[0,+infty]'} else if (f_type==1) {omec=pi*f_para;hd=sin((n-M)*omec)/((n-M)*pi);hd=c(hd,omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='LPF';fparamstr=paste0('[0, ',toString(f_para[1]),'pi]')} else if (f_type==2) {omec=pi*f_para;hd=-sin((n-M)*omec)/((n-M)*pi);hd=c(hd,1-omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='HPF';fparamstr=paste0('[',toString(f_para[1]),'pi, +infty]')} else if (f_type==3) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BPF';fparamstr=paste0('[',toString(f_para[1]),'pi, ',toString(f_para[2]),'pi]')} else if (f_type==4) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec1)-sin((n-M)*omec2))/((n-M)*pi);hd=c(hd,1-(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BSF';fparamstr=paste0('[',toString(f_para[1]),'pi,',toString(f_para[2]),'pi]')};if (w_type==0) {w=matrix(data=1,nrow=1,ncol=N);w_type_name='(rect)'} else if (w_type==1) {w=0.5*(1-cos(pi*n/M));w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hann'} else if (w_type == 2) {w=0.54-0.46*cos(pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hamm'} else if (w_type==3) {w=0.42-0.5*cos(pi*n/M)+0.08*cos(2*pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Blckmn'} else if (w_type==4) {if (is.na(b)) {b=5};w=besselI(nu=0, x=b*sqrt(1-(n/M-1)^2))/besselI(nu=0,x=b);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name=paste0('Kaiser,beta=',toString(b))};Ma(b=w*hd)",
                              `1st-order All-Pass, (cutoff=0.2), Zolz2003`="omegac=0.2;omegacprime=tan(omegac*pi/2);cc=(omegacprime-1)/(omegacprime+1);Zpg(zero=c(-1/cc), pole=c(-cc), gain=cc)",
                              `1st-order Parametric LPF, cutoff=0.2, Zolz2003`="omegac=0.2;omegacprime=tan(omegac*pi/2);cc=(omegacprime-1)/(omegacprime+1);Zpg(zero=c(-1), pole=c(-cc), gain=cc/2)",
                              `1st-order Parametric HPF, cutoff=0.2, Zolz2003`="omegac=0.2;omegacprime=tan(omegac*pi/2);cc=(omegacprime-1)/(omegacprime+1);Zpg(zero=c(1), pole=c(-cc), gain=cc/2)",
                              `1st-order Shelving (LF), Bass-boost=10dB, cutoff=0.01`="omegac=0.01;G=10;f1=omegac;f2=f1*(10^(G/20));L1=exp(-pi*f2);K1=exp(-pi*f1);b0=-1;b1=L1;a0=1;a1=K1;Arma(b=c(b0,b1),a=c(1,-a1))",
                              # `1st-order Shelving (LF), Bass-cut=-10dB, cutoff=0.01`="omegac=0.01;G=10;f1=omegac;f2=f1*(10^(G/20));L1=exp(-pi*f2);K1=exp(-pi*f1);b0=-1;b1=L1;a0=1;a1=K1;Arma(b=c(1,-a1),a=c(b0,b1))",
                              # `1st-order Shelving (HF), Treble-boost=10dB, cutoff=0.05`="omegac=0.05;G=10;f1=omegac;f2=f1*(10^(G/20));L1=exp(-pi*f2);K1=exp(-pi*f1);b0=-1;b1=L1;a0=1;a1=-K1;normalize=(1-a1)/(b0+b1);b0=normalize*b0;b1=normalize*b1;Arma(b=c(b0,b1),a=c(1,-a1))",
                              # `1st-order Shelving (HF), Treble-cut=-10dB, cutoff=0.05`="omegac=0.8;G=-12;f1=omegac;f2=f1*(10^(G/20));L1=exp(-pi*f2);K1=exp(-pi*f1);b0=-1;b1=L1;a0=1;a1=-K1;normalize=(1-a1)/(b0+b1);b0=normalize*b0;b1=normalize*b1;Arma(b=c(1,-a1),a=c(b0,b1))",
                              `1st-order Shelving (LF), Bass-boost=10dB, cutoff=0.01, Orfanidis 2010`="omegac=0.01;G=10^(10/20);G0=10^(0/20);Gc=sqrt((G^2)/2);beta=sqrt(((Gc^2)-(G0^2))/((G^2)-(Gc^2)))*(tan(pi*omegac/2));b0=(G0+((G*beta))/(1+beta));b1=(G0-((G*beta))/(1+beta));a1=(1-beta)/(1+beta);Arma(b=c(b0,-b1),a=c(1,-a1))",
                              `2nd-order All-Pass, (cutoff=0.2, bandwidth=0.022), Zolz2003`="omegac=0.2;omegab=0.022;omegabprime=tan(omegab*pi);cc=(omegabprime-1)/(omegabprime+1);dd=-cos(pi*omegac);bvectr=c(-cc,dd*(1-cc),1);Arma(b=bvectr,a=rev(bvectr))",
                              `2nd-order Parametric BPF, center=0.2, bandwidth=0.022, Zolz2003`="omegac=0.2;omegab=0.022;omegabprime=tan(omegab*pi);cc=(omegabprime-1)/(omegabprime+1);dd=-cos(pi*omegac);Zpg(zero=c(1,-1),pole=c((-dd*(1-cc)+sqrt(dd^2*(1-cc)^2+4*cc+0i))/2,(-dd*(1-cc)-sqrt(dd^2*(1-cc)^2+4*cc+0i))/2),gain=(1+cc)/2)",
                              `2nd-order Parametric BSF, center=0.2, bandwidth=0.022, Zolz2003`="omegac=0.2;omegab=0.022;omegabprime=tan(omegab*pi);cc=(omegabprime-1)/(omegabprime+1);dd=-cos(pi*omegac);Zpg(zero=c(-dd+sqrt(dd^2-1+0i),-dd-sqrt(dd^2-1+0i)),pole=c((-dd*(1-cc)+sqrt(dd^2*(1-cc)^2+4*cc+0i))/2,(-dd*(1-cc)-sqrt(dd^2*(1-cc)^2+4*cc+0i))/2),gain=(1-cc)/2)",
                              `2nd-order Parametric LPF, cutoff=0.3, Zolz97`="omegac=0.3;K=tan(pi*omegac/2);b0=K^2/(1+sqrt(2)*K+K^2);a1=2*(K^2-1)/(1+sqrt(2)*K+K^2);a2=(1-sqrt(2)*K+K^2)/(1+sqrt(2)*K+K^2);Arma(b=c(b0,2*b0,b0),a=c(1,a1,a2))",
                              `2nd-order Parametric HPF, cutoff=0.7, Zolz97`="omegac=0.7;K=tan(pi*omegac/2);b0=1/(1+sqrt(2)*K+K^2);a1=2*(K^2-1)/(1+sqrt(2)*K+K^2);a2=(1-sqrt(2)*K+K^2)/(1+sqrt(2)*K+K^2);Arma(b=c(b0,-2*b0,b0),a=c(1,a1,a2))",
                              `2nd-order Shelving-filter (LF), Bass-boost=15dB, cutoff=0.3, Zolz97`="omegac=0.3; G=15; K=tan(pi*omegac/2);boostV0=10^(G/20); b0=(1+sqrt(2*boostV0)*K+boostV0*K^2)/(1+sqrt(2)*K+K^2); b1=2*(boostV0*K^2-1)/(1+sqrt(2)*K+K^2); b2=(1-sqrt(2*boostV0)*K+boostV0*K^2)/(1+sqrt(2)*K+K^2); a1=2*(K^2-1)/(1+sqrt(2)*K+K^2); a2=(1-sqrt(2)*K+K^2)/(1+sqrt(2)*K+K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                              `2nd-order Shelving-filter (LF), Bass-cut= -15dB, cutoff=0.3, Zolz97`="omegac=0.3; G= -15; K=tan(pi*omegac/2);cutV0=10^(-G/20); b0=(1+sqrt(2)*K+K^2)/(1+sqrt(2*cutV0)*K+cutV0*K^2); b1=2*(K^2-1)/(1+sqrt(2*cutV0)*K+cutV0*K^2); b2=(1-sqrt(2)*K+K^2)/(1+sqrt(2*cutV0)*K+cutV0*K^2); a1=2*(cutV0*K^2-1)/(1+sqrt(2*cutV0)*K+cutV0*K^2); a2=(1-sqrt(2*cutV0)*K+cutV0*K^2)/(1+sqrt(2*cutV0)*K+cutV0*K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                              `2nd-order Shelving-filter (HF), Treble-boost=15dB, cutoff=0.7, Zolz97`="omegac=0.7; G=15; K=tan(pi*omegac/2);boostV0=10^(G/20); b0=(boostV0+sqrt(2*boostV0)*K+K^2)/(1+sqrt(2)*K+K^2); b1=2*(K^2-boostV0)/(1+sqrt(2)*K+K^2); b2=(boostV0-sqrt(2*boostV0)*K+K^2)/(1+sqrt(2)*K+K^2); a1=2*(K^2-1)/(1+sqrt(2)*K+K^2); a2=(1-sqrt(2)*K+K^2)/(1+sqrt(2)*K+K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                              `2nd-order Shelving-filter (HF), Treble-cut= -15dB, cutoff=0.7, Zolz97`="omegac=0.7; G= -15; K=tan(pi*omegac/2);cutV0=10^(-G/20); b0=(1+sqrt(2)*K+K^2)/(cutV0+sqrt(2*cutV0)*K+cutV0*K^2); b1=2*(K^2-1)/(cutV0+sqrt(2*cutV0)*K+cutV0*K^2); b2=(1-sqrt(2)*K+K^2)/(cutV0+sqrt(2*cutV0)*K+cutV0*K^2); a1=2*(K^2/cutV0-1)/(1+sqrt(2/cutV0)*K+K^2/cutV0); a2=(1-sqrt(2/cutV0)*K+K^2/cutV0)/(1+sqrt(2/cutV0)*K+K^2/cutV0); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                              `2nd-order Peaking-filter (equalizer), boost=15dB, center=0.3, Q=1.25, Zolz97`="omegac=0.3; Q=1.25; G=15; K=tan(pi*omegac/2);boostV0=10^(G/20); b0=(1+(boostV0/Q)*K+K^2)/(1+(1/Q)*K+K^2); b1=2*(K^2-1)/(1+(1/Q)*K+K^2); b2=(1-(boostV0/Q)*K+K^2)/(1+(1/Q)*K+K^2); a1=b1; a2=(1-(1/Q)*K+K^2)/(1+(1/Q)*K+K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                              `2nd-order Notch-filter (equalizer), cut= -15dB, center=0.3, Q=1.25, Zolz97`="omegac=0.3; Q=1.25; G= -15; K=tan(pi*omegac/2);cutV0  =10^(-G/20); b0=(1+(1/Q)*K+K^2)/(1+(cutV0/Q)*K+K^2); b1=2*(K^2-1)/(1+(cutV0/Q)*K+K^2); b2=(1-(1/Q)*K+K^2)/(1+(cutV0/Q)*K+K^2); a1=b1; a2=(1-(cutV0/Q)*K+K^2)/(1+(cutV0/Q)*K+K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                              `Remez (Parks-McClellan optimal/ equiripple/ minimax FIR), order=15, length=16, LPF, 0.3` = "L=16;remez(n=L-1, f= c(0, 0.3, 0.4, 1), a= c(1,1, 0,0), ftype= \"bandpass\")",
                              `Remez, ord. 30, symmetrical FIR (J. Dobes, 2003 Ex1)`="firstHalfCoef=c(0.034025544,0.006219216,-0.005305575,0.006128687,-0.005593423,0.00624262,-0.006848848,0.008979105,-0.008978654,0.017501073,-0.006953636,0.039774499,-0.064655981,0.085240952,-0.131292156);Ma(b=c(firstHalfCoef,0.195140968,rev(firstHalfCoef)))",
                              `Remez, ord. 48, symmetrical FIR (J. Dobes, 2003 Ex2)`="firstHalfCoef=c(0.00012511398639,0.00001335284427,0.00016015250121,0.00000634686622,0.00026201837991,0.00007281852105,0.00045629795460,0.00022819555936,0.00071588589103,0.00047316021190,0.00110189764986,0.00088563032407,0.00184819117706,0.00188050116629,0.00282392666400,0.00363976768981,0.00591155524557,0.00644303257612,0.01406190034797,0.00537162176461,0.03594691432517,0.06164502638211,0.08276620944465,0.13009560635626);Ma(b=c(firstHalfCoef,0.19452719610477,rev(firstHalfCoef)))",
                              `Remez, ord. 48, psychoacoustic/ physiological volume-control FIR (Dobes, 2003 Ex2)`="firstHalfZeros=c(-1.27851808211318+0.62684824819101i,-1.36614299238589+0.27717114439577i,-1.07284006705681+0.91235620647926i,-0.77267104757162+1.12541933041583i,-0.44961436355736+1.22363585379352i,-0.20330452525073+1.29600355151311i,0.14750702212683+1.34841369533889i,0.50634734941036+1.25979827310114i,0.83329614995319+1.04982507756824i,1.04560310563666+0.67293837999034i,1.20771535897979+0.24059979456907i,0.95139216401989+0.56315210274529i,0.77837030003487+0.46073626392613i,0.79640192035334+0.15865836018951i,0.67627030631208+0.43523995090672i,0.46384030886663+0.58436750039291i,0.27466939387352+0.68338074343342i,0.08016782642482+0.73284236586261i,-0.11813451163119+0.75307102211071i,-0.26456653442537+0.72002392155667i,-0.41461446573694+0.60389882069075i,-0.70304843505842+0.14263861132902i,-0.63057376627246+0.30916579614562i,-0.54091513103870+0.46000078868751i);Zpg(zero=c(firstHalfZeros,Conj(firstHalfZeros)),pole=c(0),gain=0.00012511398639)",
                              `Irregular IIR by Chained-Fractions (unstable) (J. Dobes, 2003 Ex3)`="Zpg(zero=c(0.9049098+0.1414979i,0.9049098-0.1414979i,1.192327), pole=c(0.9588639+0.7240575i,0.9588639-0.7240575i,1.511628), gain=1)",
                              `Savitzky-Golay smoothing-filter, FIR, order 3 (cubic), length 5` = "p=3;n=2;sgolay(p,(2*n+1)) %*% c(1,rep(0,times=(2*n+1)-1))", # https://en.wikipedia.org/wiki/Savitzky%E2%80%93Golay_filter
                              `Pink-noise filter, 1/f power-law, 3rd-order IIR, (J. Smith, 2011)`="Arma(b=c(0.049922035,-0.095993537,0.050612699,-0.004408786),a=c(1,-2.494956002,2.017265875,-0.522189400))",
                              `Pink-noise filter, 1/f power-law, 3rd-order IIR, (RBJ, 1998)`="Zpg(pole=c(0.99572754,0.94790649,0.53567505),zero=c(0.98443604,0.83392334,0.07568359),gain=1)",
                              `Wiener adaptive FIR-filter example, transversal, two-taps, noisy channel, Haykin 1998 (WSL)`="Arma(b=c(1,-0.939137),a=c(1,-0.1,-0.8))",
                              `Raised-cosine pulse-shaper, order 25, rolloff=0.25, span=6, Samples/Symbol=4`="Ma(b=c(-0.01877334400452212,0.0030135586664219974,0.032677234546254562,0.047093583340252425,0.026549517702290821,-0.027522224030017681,-0.085224875017083229,-0.099447435991926181,-0.03214726739831468,0.11903714817925418,0.31117641155547854,0.47200389556543276,0.53463207017815584,0.47200389556543276,0.31117641155547854,0.11903714817925418,-0.03214726739831468,-0.099447435991926181,-0.085224875017083229,-0.027522224030017681,0.026549517702290821,0.047093583340252425,0.032677234546254562,0.0030135586664219974,-0.01877334400452212))", # beta=0.25;m=12;n=4;b=rep(1,times=2*m);for (k in c(0:(2*m))) {knm=((k/n)-m+eps);b[k+1]=(sin(pi*knm)/(pi*knm))*(cos(beta*pi*knm)/(1-4*beta*beta*knm*knm))};b
                              `Fractional-grpdelay, Thiran all-pass (Bessel max-flat grpdelay), grpdelay=2.4 Samples, ord. 3`="D=2.4;N=3;a=rep(1,times=N);for (k in (0:N)) {a[k+1]=(-1)^k*choose(N,k)*{accu=1;for (i in 0:N) {accu=accu*(((D-N+i)/(D-N+k+i)))};accu}};Arma(b=rev(a),a=a)", # "Zpg(zero=c(6.59457337747154+10.2534695974275i,6.59457337747154-10.2534695974275i,-1.61771818351450),pole=c(-0.618154639164343,0.0443714372292301+0.0689902373636964i,0.0443714372292301-0.0689902373636964i),gain=0.00415923945335710)",
                              # `Room Impulse-Response (McGovern 2003)`="fs=16000;mic=c(19,18,1.6);n=6;r=0.3;rm=c(20,19,21);src=c(5,2,1);nn=(-n):n;rms=nn+0.5-0.5*(-1)^nn;srcs=(-1)^(nn);xi=srcs*src[1]+rms*rm[1]-mic[1];yj=srcs*src[2]+rms*rm[2]-mic[2];zk=srcs*src[3]+rms*rm[3]-mic[3];rv=matlab::meshgrid(xi,yj,zk,nargout=3);i=rv$x;j=rv$y;k=rv$z;d=sqrt(i^2+j^2+k^2);time=round(fs*d/343)+1;rv=matlab::meshgrid(nn,nn,nn,nargout=3);e=rv$x;f=rv$y;g=rv$z;c=r^(abs(e)+abs(f)+abs(g));e=c/d;h=Matrix::sparseMatrix(i=as.vector(time),j=rep(1,times=length(as.vector(time))),x=as.vector(e));b=as.vector(h[1:min(c(3000,length(h)))]);Ma(b=c(1,b)/max(abs(b)))",
                              `Dolph-Chebyshev window, 50-point, 100dB attenuation` = "chebwin(n=50, at=100)",
                              `Kaiser-window, 101-point, beta 0 (very-wide=Rectangle)` = "kaiser(n=101, beta=0)",
                              `Kaiser-window, 101-point, beta 50 (narrower)` = "kaiser(n=101, beta=50)",
                              `Kaiser-windowed FIR, LPF, 0.3, minimum-order` = "with(kaiserord(f=c(0.275,0.325), m=c(1,0), dev=c(0.1,0.1)),fir1(n=n,w=Wc,type=type,window=kaiser(n+1,beta),scale=FALSE))",
                              `Rectangle (FIR), 101-point, given b` = "Ma(b=rep(1,times=101))",
                              `Bartlett-window, 41-point` = "bartlett(41)",
                              `Blackman-window, 41-point` = "blackman(41)",
                              `Boxcar-window (aka. Rectangular, Dirichlet), 41-point` = "boxcar(41)",
                              `Flattop-window, symmetric, 41-point` = "flattopwin(41,sym=\"symmetric\")",
                              `Flattop-window, periodic (DFT Even), 41-point` = "flattopwin(41,sym=\"periodic\")",
                              `Gaussian-window, 41-point` = "sd=0.2;gausswin(41,w=1/sd)", # https://en.wikipedia.org/wiki/Window_function#Gaussian_window
                              `vonHann(ing)-window (raised-cosine, sine-squared), 41-point` = "hanning(41)",
                              `Hamming-window, 41-point` = "hamming(41)",
                              `Triangle-window (Bartlett, but no zero-endpoint), 41-point` = "triang(41)",
                              `Windowed-Sinc (e.g. using Hamming), 19-point, 0.3` = "N=19;leftside=sin(pi*(0.3*(-(N/2):(-1))))/(pi*(0.3*(-(N/2):(-1))));c(leftside,1,rev(leftside))*hamming(N+1)", # "N=18;sinc(0.3*(-(N/2):(N/2)))*0.3*blackman(N+1)",
                              `Spencer 15-point Moving-Average FIR` = "spencerFilter()",
                              `Spencer 15-point MA FIR, given b` = "Ma(b=c(-3, -6, -5, 3, 21, 46, 67, 74, 67, 46, 21, 3, -5, -6, -3) / 320)",
                              `( random-filter from this list )` = paste0(
                                "sample(c('",
                                paste(
                                  "N=40;fir1(n=N,w=0.3,type=\"low\",window=hamming(N+1),scale=TRUE)",
                                  "N=10;fir1(n=N,w=c(0.3,0.65),type=\"pass\",window=hamming(N+1),scale=TRUE)",
                                  "N=10;fir1(n=N,w=c(0.3,0.65),type=\"stop\",window=hamming(N+1),scale=TRUE)",
                                  "fir2(n=100, f=c(0, 0.3, 0.3, 0.65, 0.65, 1), m=c(0, 0, 1, 0.5, 0, 0), grid_n=512, ramp_n=5, window=hamming(101))",
                                  "Ma(b=c(1/3,2/3,1/3))",
                                  "Arma(b=c(1/3,2/3,1/3), a=c(1,1-eps))",
                                  "Zpg(zero=c(-1,-1), pole=c(-(1-eps)), gain=1/3)",
                                  "Ma(b=c(1,-fpmaxx))",
                                  "Zpg(zero=c(0), pole=c(-0.92345,0.92345), gain=1)",
                                  "Zpg(zero=c(0), pole=c(-0.5+0.52345i,-0.5-0.52345i), gain=1)",
                                  "Zpg(zero=c(0), pole=c(0.92345i,-0.92345i), gain=1)",
                                  "Zpg(zero=c(0), pole=c(-0.92345,-0.92345), gain=1)",
                                  "omegac=0.4;omegacprime=tan(omegac*pi/2);Zpg(zero=c(-1), pole=c(-(omegacprime-1)/(omegacprime+1)), gain=omegacprime/(omegacprime+1))",
                                  "omegac=0.4;signal::bilinear(Sz=c(-fpmaxx/10),Sp=c(-tan(pi*omegac/2)),Sg=tan(pi*omegac/2)/(fpmaxx/10),T=2)",
                                  "omegac1=0.3;omegac2=0.65;signal::sftrans(Sz=c(-1e16),Sp=c(-tan(pi*omegac1/2)),Sg=tan(pi*omegac1/2)/(1e16),W=c(omegac1,omegac2),stop=FALSE)",
                                  "cheby1(n=5,Rp=3,W=0.65,type=\"high\")",
                                  "cheby1(n=5,Rp=3,W=c(0.3,0.65),type=\"pass\")",
                                  "cheby1(n=5,Rp=3,W=c(0.3,0.65),type=\"stop\")",
                                  "Arma(b=c(1), a=c(1,0,(1-eps)))",
                                  "theta=input$slider1;Zpg(zero=c(0), pole=0.99999999999999*c(exp(theta*pi*1i),exp(-theta*pi*1i)), gain=1)",
                                  "FftFilter(rep(1/5,times=5),n=512)$b",
                                  "N=5;Arma(b=c(1/N,rep(0,times=N-1),-1/N),a=c(1,-(1-eps)))",
                                  "N=450;Arma(b=c(1/N,rep(0,times=N-1),-1/N),a=c(1))",
                                  "R=5;M=1;Arma(b=c(1,rep(0,times=R*M-1),-1), a=c(1,-(1-eps)))",
                                  "N=8;Zpg(zero=c(1,-1,1i,-1i,1/sqrt(2)+1/sqrt(2)*1i,1/sqrt(2)-1/sqrt(2)*1i,-1/sqrt(2)+1/sqrt(2)*1i,-1/sqrt(2)-1/sqrt(2)*1i), pole=c(1-eps), gain=1/N)",
                                  "Arma(b=c(1,0,0, 0.5^3), a=c(1,0,0,0,0, 0.9^5))",
                                  "K=8;alpha=0.9;Arma(b=c(1), a=c(1,rep(0,times=K-1),alpha))",
                                  "K=8;alpha=-1;Arma(b=c(1,rep(0,times=K-1),alpha), a=c(1))",
                                  "Arma(b=c(1,1), a=c(1,-(1-eps)))",
                                  "Zpg(zero=c(1), pole=c(-(1-eps)), gain=1)",
                                  "omega0=0.22*pi;D=2*pi/omega0;rho=0.99;Arma(b=c(1,rep(0,times=floor(D-1)),-1), a=c(1,rep(0,times=floor(D-1)),-(rho)^D))",
                                  "theta=input$slider1;rp=0.999;rz=0.997;Zpg(zero=c(rz*exp(theta*pi*1i),rz*exp(-theta*pi*1i)), pole=c(rp*exp(theta*pi*1i),rp*exp(-theta*pi*1i)), gain=1)",
                                  "theta=input$slider1;rp=0.997;rz=0.999;Zpg(zero=c(rz*exp(theta*pi*1i),rz*exp(-theta*pi*1i)), pole=c(rp*exp(theta*pi*1i),rp*exp(-theta*pi*1i)), gain=1)",
                                  "N=12;D=7;fc=0.65;L=2*N;FF=matrix(data=0,nrow=N,ncol=1);for (k in seq(1,N,by=1)){f=2*k/(L+1);if (f <= fc) {FF[k]=exp(-1i*D*f*pi)}};Fb=Conj(FF);FF=c(1,FF,pracma::flipud(Fb));h=Re(pracma::ifft(FF));r1=t(h);r=c(r1[1],pracma::fliplr(as.matrix(t(r1[2:length(r1)]))));H=pracma::Toeplitz(h,r);H0=H[,1:(N+1)];H1=H0[1:(N+1),];h1=H0[seq((N+2),(L+1)),1];H2=H0[seq((N+2),(L+1)),seq(2,(N+1))];ah=-pracma::inv(H2) %*% h1;a=c(1,ah);b=H1 %*% a;Arma(a=a,b=b)",
                                  "N=12;D=7;L=120;fc=0.65;L1=0.5*L;FF=matrix(data=0,nrow=L1,ncol=1);for (k in seq(1,L1)){f=2*k/(L+1);if (f <= fc) {FF[k]=exp(-1i*D*f*pi)}};Fb=Conj(FF);FF=c(1,FF,pracma::flipud(Fb));h=Re(pracma::ifft(FF));r1=t(h);r=c(r1[1],pracma::fliplr(as.matrix(t(r1[2:length(r1)]))));H=pracma::Toeplitz(h,r);H0=H[,1:(N+1)];H1=H0[1:(N+1),];h1=H0[seq((N+2),(L+1)),1];H2=H0[seq((N+2),(L+1)),seq(2,(N+1))];ah=-pracma::inv(t(H2) %*% H2) %*% t(H2) %*% h1;a=c(1,ah);b=H1 %*% a;Arma(a=a,b=b)",
                                  "r1=1.3;theta=0.6;Zpg(zero=c(r1*exp(theta*pi*1i),r1*exp(-theta*pi*1i)), pole=c((1/r1)*exp(theta*pi*1i),(1/r1)*exp(-theta*pi*1i)), gain=1)",
                                  "Arma(b=c(1,-fpmaxx), a=c(1))",
                                  "myb=c(1,2,3,4,5,6);Arma(b=myb, a=rev(myb))",
                                  "Zpg(zero=c(0.9*exp(0.6*pi*1i),0.9*exp(-0.6*pi*1i),0.8*exp(0.8*pi*1i),0.8*exp(-0.8*pi*1i)), pole=c(0), gain=1)",
                                  "Zpg(zero=c(1/0.9*exp(0.6*pi*1i),1/0.9*exp(-0.6*pi*1i),1/0.8*exp(0.8*pi*1i),1/0.8*exp(-0.8*pi*1i)), pole=c(0), gain=1)",
                                  "N=41;M=20;hz=matrix(data=0,nrow=1,ncol=M);zw=seq(1,(M-1),by=2);hz[seq(1,(M-1),by=2)]=2/ (pi*zw);hd=c(-pracma::fliplr(as.matrix(hz)),0,hz);w=signal::hamming(N);hd*w",
                                  "t=seq(0,2-1/512,by=1/512);fs=512;Ts=1/fs;N=23;M=(N-1)/2;n=1:M;h=cos(n*pi)/(Ts*n);h=c(-pracma::fliplr(as.matrix(t(h))),0,h);win=signal::hamming(N);win*h",
                                  "fs=512;Ts=1/fs;N1=23;M=(N1-1)/2;n=0:(M-1);k=M-n;k2=k^2;fc=0.3*pi;h1=sin(k*fc);h2=(fc*k)*cos(k*fc);hd=(h1-h2)/(Ts*pi*k2);hd=c(hd,0,-pracma::fliplr(as.matrix(t(hd))));win=signal::hamming(N1);win*hd",
                                  "butter(n=5,W=0.3,type=\"low\")",
                                  "butter(n=5,W=0.65,type=\"high\")",
                                  "butter(n=5,W=c(0.3,0.65),type=\"pass\")",
                                  "butter(n=5,W=c(0.3,0.65),type=\"stop\")",
                                  "cheby2(n=5,Rp=20,W=0.3,type=\"low\")",
                                  "cheby2(n=5,Rp=20,W=0.65,type=\"high\")",
                                  "cheby1(n=5,Rp=3,W=0.3,type=\"low\")", # this 48th-slot seems to be the first to always be "randomly" chosen with any new freshly-started RStudio process
                                  "cheby2(n=5,Rp=20,W=c(0.3,0.65),type=\"pass\")",
                                  "cheby2(n=5,Rp=20,W=c(0.3,0.65),type=\"stop\")",
                                  "ellip(n=5,Rp=3,Rs=40,W=0.3,type=\"low\")",
                                  "ellip(n=5,Rp=3,Rs=40,W=0.65,type=\"high\")",
                                  "ellip(n=5,Rp=3,Rs=40,W=c(0.3,0.65),type=\"pass\")",
                                  "ellip(n=5,Rp=3,Rs=40,W=c(0.3,0.65),type=\"stop\")",
                                  "butter(n=buttord(Wp=0.285, Ws=0.345, Rp=0.5, Rs=29))",
                                  "cheby1(n=cheb1ord(Wp=0.3, Ws=0.34, Rp=0.5, Rs=29))",
                                  "ellip(n=ellipord(Wp=0.3, Ws=0.34, Rp=0.5, Rs=29))",
                                  "omegac=0.3;zeta=0.7;C=1/(tan(pi*omegac/2));b0=1/(1+2*zeta*C+C^2);Arma(b=c(b0,2*b0,b0),a=c(1,2*b0*(1-C^2),b0*(1-2*zeta*C+C^2)))",
                                  # "L=30;N=L-1;f_type=1;f_para=0.6;w_type=2;M=(N-1)/2;n=0:(M-1);if (f_type==0) {omec1=0;omec2=pi;hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='all-pass';fparamstr='[0,+infty]'} else if (f_type==1) {omec=pi*f_para;hd=sin((n-M)*omec)/((n-M)*pi);hd=c(hd,omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='LPF';fparamstr=paste0('[0, ',toString(f_para[1]),'pi]')} else if (f_type==2) {omec=pi*f_para;hd=-sin((n-M)*omec)/((n-M)*pi);hd=c(hd,1-omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='HPF';fparamstr=paste0('[',toString(f_para[1]),'pi, +infty]')} else if (f_type==3) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BPF';fparamstr=paste0('[',toString(f_para[1]),'pi, ',toString(f_para[2]),'pi]')} else if (f_type==4) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec1)-sin((n-M)*omec2))/((n-M)*pi);hd=c(hd,1-(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BSF';fparamstr=paste0('[',toString(f_para[1]),'pi,',toString(f_para[2]),'pi]')};if (w_type==0) {w=matrix(data=1,nrow=1,ncol=N);w_type_name='(rect)'} else if (w_type==1) {w=0.5*(1-cos(pi*n/M));w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hann'} else if (w_type == 2) {w=0.54-0.46*cos(pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hamm'} else if (w_type==3) {w=0.42-0.5*cos(pi*n/M)+0.08*cos(2*pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Blckmn'} else if (w_type==4) {if (is.na(b)) {b=5};w=besselI(nu=0, x=b*sqrt(1-(n/M-1)^2))/besselI(nu=0,x=b);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name=paste0('Kaiser,beta=',toString(b))};Ma(b=w*hd)",
                                  # "L=70;N=L-1;f_type=3;f_para=c(0.35,0.65);w_type=4;b=5;M=(N-1)/2;n=0:(M-1);if (f_type==0) {omec1=0;omec2=pi;hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='all-pass';fparamstr='[0,+infty]'} else if (f_type==1) {omec=pi*f_para;hd=sin((n-M)*omec)/((n-M)*pi);hd=c(hd,omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='LPF';fparamstr=paste0('[0, ',toString(f_para[1]),'pi]')} else if (f_type==2) {omec=pi*f_para;hd=-sin((n-M)*omec)/((n-M)*pi);hd=c(hd,1-omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='HPF';fparamstr=paste0('[',toString(f_para[1]),'pi, +infty]')} else if (f_type==3) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BPF';fparamstr=paste0('[',toString(f_para[1]),'pi, ',toString(f_para[2]),'pi]')} else if (f_type==4) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec1)-sin((n-M)*omec2))/((n-M)*pi);hd=c(hd,1-(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BSF';fparamstr=paste0('[',toString(f_para[1]),'pi,',toString(f_para[2]),'pi]')};if (w_type==0) {w=matrix(data=1,nrow=1,ncol=N);w_type_name='(rect)'} else if (w_type==1) {w=0.5*(1-cos(pi*n/M));w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hann'} else if (w_type == 2) {w=0.54-0.46*cos(pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hamm'} else if (w_type==3) {w=0.42-0.5*cos(pi*n/M)+0.08*cos(2*pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Blckmn'} else if (w_type==4) {if (is.na(b)) {b=5};w=besselI(nu=0, x=b*sqrt(1-(n/M-1)^2))/besselI(nu=0,x=b);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name=paste0('Kaiser,beta=',toString(b))};Ma(b=w*hd)",
                                  # "L=72;N=L-1;f_type=4;f_para=c(0.3,0.7);w_type=4;b=5;M=(N-1)/2;n=0:(M-1);if (f_type==0) {omec1=0;omec2=pi;hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='all-pass';fparamstr='[0,+infty]'} else if (f_type==1) {omec=pi*f_para;hd=sin((n-M)*omec)/((n-M)*pi);hd=c(hd,omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='LPF';fparamstr=paste0('[0, ',toString(f_para[1]),'pi]')} else if (f_type==2) {omec=pi*f_para;hd=-sin((n-M)*omec)/((n-M)*pi);hd=c(hd,1-omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='HPF';fparamstr=paste0('[',toString(f_para[1]),'pi, +infty]')} else if (f_type==3) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BPF';fparamstr=paste0('[',toString(f_para[1]),'pi, ',toString(f_para[2]),'pi]')} else if (f_type==4) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec1)-sin((n-M)*omec2))/((n-M)*pi);hd=c(hd,1-(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BSF';fparamstr=paste0('[',toString(f_para[1]),'pi,',toString(f_para[2]),'pi]')};if (w_type==0) {w=matrix(data=1,nrow=1,ncol=N);w_type_name='(rect)'} else if (w_type==1) {w=0.5*(1-cos(pi*n/M));w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hann'} else if (w_type == 2) {w=0.54-0.46*cos(pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hamm'} else if (w_type==3) {w=0.42-0.5*cos(pi*n/M)+0.08*cos(2*pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Blckmn'} else if (w_type==4) {if (is.na(b)) {b=5};w=besselI(nu=0, x=b*sqrt(1-(n/M-1)^2))/besselI(nu=0,x=b);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name=paste0('Kaiser,beta=',toString(b))};Ma(b=w*hd)",
                                  "omegac=0.2;omegacprime=tan(omegac*pi/2);cc=(omegacprime-1)/(omegacprime+1);Zpg(zero=c(-1/cc), pole=c(-cc), gain=cc)",
                                  "omegac=0.2;omegacprime=tan(omegac*pi/2);cc=(omegacprime-1)/(omegacprime+1);Zpg(zero=c(-1), pole=c(-cc), gain=cc/2)",
                                  "omegac=0.2;omegacprime=tan(omegac*pi/2);cc=(omegacprime-1)/(omegacprime+1);Zpg(zero=c(1), pole=c(-cc), gain=cc/2)",
                                  "omegac=0.2;omegab=0.022;omegabprime=tan(omegab*pi);cc=(omegabprime-1)/(omegabprime+1);dd=-cos(pi*omegac);bvectr=c(-cc,dd*(1-cc),1);Arma(b=bvectr,a=rev(bvectr))",
                                  "omegac=0.2;omegab=0.022;omegabprime=tan(omegab*pi);cc=(omegabprime-1)/(omegabprime+1);dd=-cos(pi*omegac);Zpg(zero=c(1,-1),pole=c((-dd*(1-cc)+sqrt(dd^2*(1-cc)^2+4*cc+0i))/2,(-dd*(1-cc)-sqrt(dd^2*(1-cc)^2+4*cc+0i))/2),gain=(1+cc)/2)",
                                  "omegac=0.2;omegab=0.022;omegabprime=tan(omegab*pi);cc=(omegabprime-1)/(omegabprime+1);dd=-cos(pi*omegac);Zpg(zero=c(-dd+sqrt(dd^2-1+0i),-dd-sqrt(dd^2-1+0i)),pole=c((-dd*(1-cc)+sqrt(dd^2*(1-cc)^2+4*cc+0i))/2,(-dd*(1-cc)-sqrt(dd^2*(1-cc)^2+4*cc+0i))/2),gain=(1-cc)/2)",
                                  "omegac=0.3;K=tan(pi*omegac/2);b0=K^2/(1+sqrt(2)*K+K^2);a1=2*(K^2-1)/(1+sqrt(2)*K+K^2);a2=(1-sqrt(2)*K+K^2)/(1+sqrt(2)*K+K^2);Arma(b=c(b0,2*b0,b0),a=c(1,a1,a2))",
                                  "omegac=0.7;K=tan(pi*omegac/2);b0=1/(1+sqrt(2)*K+K^2);a1=2*(K^2-1)/(1+sqrt(2)*K+K^2);a2=(1-sqrt(2)*K+K^2)/(1+sqrt(2)*K+K^2);Arma(b=c(b0,-2*b0,b0),a=c(1,a1,a2))",
                                  "omegac=0.3;G=15; K=tan(pi*omegac/2);boostV0=10^(G/20); b0=(1+sqrt(2*boostV0)*K+boostV0*K^2)/(1+sqrt(2)*K+K^2); b1=2*(boostV0*K^2-1)/(1+sqrt(2)*K+K^2); b2=(1-sqrt(2*boostV0)*K+boostV0*K^2)/(1+sqrt(2)*K+K^2); a1=2*(K^2-1)/(1+sqrt(2)*K+K^2); a2=(1-sqrt(2)*K+K^2)/(1+sqrt(2)*K+K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                                  "omegac=0.3;G=-15; K=tan(pi*omegac/2);cutV0  =10^(-G/20); b0=(1+sqrt(2)*K+K^2)/(1+sqrt(2*cutV0)*K+cutV0*K^2); b1=2*(K^2-1)/(1+sqrt(2*cutV0)*K+cutV0*K^2); b2=(1-sqrt(2)*K+K^2)/(1+sqrt(2*cutV0)*K+cutV0*K^2); a1=2*(cutV0*K^2-1)/(1+sqrt(2*cutV0)*K+cutV0*K^2); a2=(1-sqrt(2*cutV0)*K+cutV0*K^2)/(1+sqrt(2*cutV0)*K+cutV0*K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                                  "omegac=0.7;G=15; K=tan(pi*omegac/2);boostV0=10^(G/20); b0=(boostV0+sqrt(2*boostV0)*K+K^2)/(1+sqrt(2)*K+K^2); b1=2*(K^2-boostV0)/(1+sqrt(2)*K+K^2); b2=(boostV0-sqrt(2*boostV0)*K+K^2)/(1+sqrt(2)*K+K^2); a1=2*(K^2-1)/(1+sqrt(2)*K+K^2); a2=(1-sqrt(2)*K+K^2)/(1+sqrt(2)*K+K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                                  "omegac=0.7;G=-15; K=tan(pi*omegac/2);cutV0  =10^(-G/20); b0=(1+sqrt(2)*K+K^2) /(cutV0+sqrt(2*cutV0)*K+cutV0*K^2); b1=2*(K^2-1)/(cutV0+sqrt(2*cutV0)*K+cutV0*K^2); b2=(1-sqrt(2)*K+K^2)/(cutV0+sqrt(2*cutV0)*K+cutV0*K^2); a1=2*(K^2/cutV0-1)/(1+sqrt(2/cutV0)*K+K^2/cutV0); a2=(1-sqrt(2/cutV0)*K+K^2/cutV0)/(1+sqrt(2/cutV0)*K+K^2/cutV0); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                                  "omegac=0.3;Q=1.25;G=15; K=tan(pi*omegac/2);boostV0=10^(G/20); b0=(1+(boostV0/Q)*K+K^2)/(1+(1/Q)*K+K^2); b1=2*(K^2-1)/(1+(1/Q)*K+K^2); b2=(1-(boostV0/Q)*K+K^2)/(1+(1/Q)*K+K^2); a1=b1; a2=(1-(1/Q)*K+K^2)/(1+(1/Q)*K+K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                                  "omegac=0.3;Q=1.25;G=-15; K=tan(pi*omegac/2);cutV0  =10^(-G/20); b0=(1+(1/Q)*K+K^2)/(1+(cutV0/Q)*K+K^2); b1=2*(K^2-1)/(1+(cutV0/Q)*K+K^2); b2=(1-(1/Q)*K+K^2)/(1+(cutV0/Q)*K+K^2); a1=b1; a2=(1-(cutV0/Q)*K+K^2)/(1+(cutV0/Q)*K+K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                                  "L=16;remez(n=L-1, f= c(0, 0.3, 0.4, 1), a= c(1,1, 0,0), ftype= \"bandpass\")",
                                  "firstHalfCoef=c(0.034025544,0.006219216,-0.005305575,0.006128687,-0.005593423,0.00624262,-0.006848848,0.008979105,-0.008978654,0.017501073,-0.006953636,0.039774499,-0.064655981,0.085240952,-0.131292156);Ma(b=c(firstHalfCoef,0.195140968,rev(firstHalfCoef)))",
                                  "firstHalfCoef=c(0.00012511398639,0.00001335284427,0.00016015250121,0.00000634686622,0.00026201837991,0.00007281852105,0.00045629795460,0.00022819555936,0.00071588589103,0.00047316021190,0.00110189764986,0.00088563032407,0.00184819117706,0.00188050116629,0.00282392666400,0.00363976768981,0.00591155524557,0.00644303257612,0.01406190034797,0.00537162176461,0.03594691432517,0.06164502638211,0.08276620944465,0.13009560635626);Ma(b=c(firstHalfCoef,0.19452719610477,rev(firstHalfCoef)))",
                                  "firstHalfZeros=c(-1.27851808211318+0.62684824819101i,-1.36614299238589+0.27717114439577i,-1.07284006705681+0.91235620647926i,-0.77267104757162+1.12541933041583i,-0.44961436355736+1.22363585379352i,-0.20330452525073+1.29600355151311i,0.14750702212683+1.34841369533889i,0.50634734941036+1.25979827310114i,0.83329614995319+1.04982507756824i,1.04560310563666+0.67293837999034i,1.20771535897979+0.24059979456907i,0.95139216401989+0.56315210274529i,0.77837030003487+0.46073626392613i,0.79640192035334+0.15865836018951i,0.67627030631208+0.43523995090672i,0.46384030886663+0.58436750039291i,0.27466939387352+0.68338074343342i,0.08016782642482+0.73284236586261i,-0.11813451163119+0.75307102211071i,-0.26456653442537+0.72002392155667i,-0.41461446573694+0.60389882069075i,-0.70304843505842+0.14263861132902i,-0.63057376627246+0.30916579614562i,-0.54091513103870+0.46000078868751i);Zpg(zero=c(firstHalfZeros,Conj(firstHalfZeros)),pole=c(0),gain=0.00012511398639)",
                                  "Zpg(zero=c(0.9049098+0.1414979i,0.9049098-0.1414979i,1.192327), pole=c(0.9588639+0.7240575i,0.9588639-0.7240575i,1.511628), gain=1)",
                                  "p=3;n=2;sgolay(p,(2*n+1)) %*% c(1,rep(0,times=(2*n+1)-1))",
                                  "Arma(b=c(0.049922035,-0.095993537,0.050612699,-0.004408786),a=c(1,-2.494956002,2.017265875,-0.522189400))",
                                  "Zpg(pole=c(0.99572754,0.94790649,0.53567505),zero=c(0.98443604,0.83392334,0.07568359),gain=1)",
                                  "Arma(b=c(1,-0.939137),a=c(1,-0.1,-0.8))",
                                  "Ma(b=c(-0.01877334400452212,0.0030135586664219974,0.032677234546254562,0.047093583340252425,0.026549517702290821,-0.027522224030017681,-0.085224875017083229,-0.099447435991926181,-0.03214726739831468,0.11903714817925418,0.31117641155547854,0.47200389556543276,0.53463207017815584,0.47200389556543276,0.31117641155547854,0.11903714817925418,-0.03214726739831468,-0.099447435991926181,-0.085224875017083229,-0.027522224030017681,0.026549517702290821,0.047093583340252425,0.032677234546254562,0.0030135586664219974,-0.01877334400452212))",
                                  "D=2.4;N=3;a=rep(1,times=N);for (k in (0:N)) {a[k+1]=(-1)^k*choose(N,k)*{accu=1;for (i in 0:N) {accu=accu*(((D-N+i)/(D-N+k+i)))};accu}};Arma(b=rev(a),a=a)", # "Zpg(zero=c(6.59457337747154+10.2534695974275i,6.59457337747154-10.2534695974275i,-1.61771818351450),pole=c(-0.618154639164343,0.0443714372292301+0.0689902373636964i,0.0443714372292301-0.0689902373636964i),gain=0.00415923945335710)",
                                  "chebwin(n=50, at=100)",
                                  "kaiser(n=101, beta=0)",
                                  "kaiser(n=101, beta=50)",
                                  "with(kaiserord(f=c(0.275,0.325), m=c(1,0), dev=c(0.1,0.1)),fir1(n=n,w=Wc,type=type,window=kaiser(n+1,beta),scale=FALSE))",
                                  "Ma(b=rep(1,times=101))",
                                  "bartlett(41)",
                                  "blackman(41)",
                                  "boxcar(41)",
                                  "flattopwin(41,sym=\"symmetric\")",
                                  "flattopwin(41,sym=\"periodic\")",
                                  "sd=0.2;gausswin(41,w=1/sd)",
                                  "hanning(41)",
                                  "hamming(41)",
                                  "triang(41)",
                                  "N=19;leftside=sin(pi*(0.3*(-(N/2):(-1))))/(pi*(0.3*(-(N/2):(-1))));c(leftside,1,rev(leftside))*hamming(N+1)", # "N=18;sinc(0.3*(-(N/2):(N/2)))*0.3*blackman(N+1)",
                                  "spencerFilter()",
                                  "Ma(b=c(-3, -6, -5, 3, 21, 46, 67, 74, 67, 46, 21, 3, -5, -6, -3) / 320)",
                                  sep = "','"
                                ),
                                "'), 1 )"
                              )
                            ),
                            selected = sample(
                              c(
                                "N=40;fir1(n=N,w=0.3,type=\"low\",window=hamming(N+1),scale=TRUE)",
                                "N=10;fir1(n=N,w=c(0.3,0.65),type=\"pass\",window=hamming(N+1),scale=TRUE)",
                                "N=10;fir1(n=N,w=c(0.3,0.65),type=\"stop\",window=hamming(N+1),scale=TRUE)",
                                "fir2(n=100, f=c(0, 0.3, 0.3, 0.65, 0.65, 1), m=c(0, 0, 1, 0.5, 0, 0), grid_n=512, ramp_n=5, window=hamming(101))",
                                "Ma(b=c(1/3,2/3,1/3))",
                                "Arma(b=c(1/3,2/3,1/3), a=c(1,1-eps))",
                                "Zpg(zero=c(-1,-1), pole=c(-(1-eps)), gain=1/3)",
                                "Ma(b=c(1,-fpmaxx))",
                                "Zpg(zero=c(0), pole=c(-0.92345,0.92345), gain=1)",
                                "Zpg(zero=c(0), pole=c(-0.5+0.52345i,-0.5-0.52345i), gain=1)",
                                "Zpg(zero=c(0), pole=c(0.92345i,-0.92345i), gain=1)",
                                "Zpg(zero=c(0), pole=c(-0.92345,-0.92345), gain=1)",
                                "omegac=0.4;omegacprime=tan(omegac*pi/2);Zpg(zero=c(-1), pole=c(-(omegacprime-1)/(omegacprime+1)), gain=omegacprime/(omegacprime+1))",
                                "omegac=0.4;signal::bilinear(Sz=c(-fpmaxx/10),Sp=c(-tan(pi*omegac/2)),Sg=tan(pi*omegac/2)/(fpmaxx/10),T=2)",
                                "omegac1=0.3;omegac2=0.65;signal::sftrans(Sz=c(-1e16),Sp=c(-tan(pi*omegac1/2)),Sg=tan(pi*omegac1/2)/(1e16),W=c(omegac1,omegac2),stop=FALSE)",
                                "cheby1(n=5,Rp=3,W=0.65,type=\"high\")",
                                "cheby1(n=5,Rp=3,W=c(0.3,0.65),type=\"pass\")",
                                "cheby1(n=5,Rp=3,W=c(0.3,0.65),type=\"stop\")",
                                "Arma(b=c(1), a=c(1,0,(1-eps)))",
                                "theta=input$slider1;Zpg(zero=c(0), pole=0.99999999999999*c(exp(theta*pi*1i),exp(-theta*pi*1i)), gain=1)",
                                "FftFilter(rep(1/5,times=5),n=512)$b",
                                "N=5;Arma(b=c(1/N,rep(0,times=N-1),-1/N),a=c(1,-(1-eps)))",
                                "N=450;Arma(b=c(1/N,rep(0,times=N-1),-1/N),a=c(1))",
                                "R=5;M=1;Arma(b=c(1,rep(0,times=R*M-1),-1), a=c(1,-(1-eps)))",
                                "N=8;Zpg(zero=c(1,-1,1i,-1i,1/sqrt(2)+1/sqrt(2)*1i,1/sqrt(2)-1/sqrt(2)*1i,-1/sqrt(2)+1/sqrt(2)*1i,-1/sqrt(2)-1/sqrt(2)*1i), pole=c(1-eps), gain=1/N)",
                                "Arma(b=c(1,0,0, 0.5^3), a=c(1,0,0,0,0, 0.9^5))",
                                "K=8;alpha=0.9;Arma(b=c(1), a=c(1,rep(0,times=K-1),alpha))",
                                "K=8;alpha=-1;Arma(b=c(1,rep(0,times=K-1),alpha), a=c(1))",
                                "Arma(b=c(1,1), a=c(1,-(1-eps)))",
                                "Zpg(zero=c(1), pole=c(-(1-eps)), gain=1)",
                                "omega0=0.22*pi;D=2*pi/omega0;rho=0.99;Arma(b=c(1,rep(0,times=floor(D-1)),-1), a=c(1,rep(0,times=floor(D-1)),-(rho)^D))",
                                "theta=input$slider1;rp=0.999;rz=0.997;Zpg(zero=c(rz*exp(theta*pi*1i),rz*exp(-theta*pi*1i)), pole=c(rp*exp(theta*pi*1i),rp*exp(-theta*pi*1i)), gain=1)",
                                "theta=input$slider1;rp=0.997;rz=0.999;Zpg(zero=c(rz*exp(theta*pi*1i),rz*exp(-theta*pi*1i)), pole=c(rp*exp(theta*pi*1i),rp*exp(-theta*pi*1i)), gain=1)",
                                "N=12;D=7;fc=0.65;L=2*N;FF=matrix(data=0,nrow=N,ncol=1);for (k in seq(1,N,by=1)){f=2*k/(L+1);if (f <= fc) {FF[k]=exp(-1i*D*f*pi)}};Fb=Conj(FF);FF=c(1,FF,pracma::flipud(Fb));h=Re(pracma::ifft(FF));r1=t(h);r=c(r1[1],pracma::fliplr(as.matrix(t(r1[2:length(r1)]))));H=pracma::Toeplitz(h,r);H0=H[,1:(N+1)];H1=H0[1:(N+1),];h1=H0[seq((N+2),(L+1)),1];H2=H0[seq((N+2),(L+1)),seq(2,(N+1))];ah=-pracma::inv(H2) %*% h1;a=c(1,ah);b=H1 %*% a;Arma(a=a,b=b)",
                                "N=12;D=7;L=120;fc=0.65;L1=0.5*L;FF=matrix(data=0,nrow=L1,ncol=1);for (k in seq(1,L1)){f=2*k/(L+1);if (f <= fc) {FF[k]=exp(-1i*D*f*pi)}};Fb=Conj(FF);FF=c(1,FF,pracma::flipud(Fb));h=Re(pracma::ifft(FF));r1=t(h);r=c(r1[1],pracma::fliplr(as.matrix(t(r1[2:length(r1)]))));H=pracma::Toeplitz(h,r);H0=H[,1:(N+1)];H1=H0[1:(N+1),];h1=H0[seq((N+2),(L+1)),1];H2=H0[seq((N+2),(L+1)),seq(2,(N+1))];ah=-pracma::inv(t(H2) %*% H2) %*% t(H2) %*% h1;a=c(1,ah);b=H1 %*% a;Arma(a=a,b=b)",
                                "r1=1.3;theta=0.6;Zpg(zero=c(r1*exp(theta*pi*1i),r1*exp(-theta*pi*1i)), pole=c((1/r1)*exp(theta*pi*1i),(1/r1)*exp(-theta*pi*1i)), gain=1)",
                                "Arma(b=c(1,-fpmaxx), a=c(1))",
                                "myb=c(1,2,3,4,5,6);Arma(b=myb, a=rev(myb))",
                                "Zpg(zero=c(0.9*exp(0.6*pi*1i),0.9*exp(-0.6*pi*1i),0.8*exp(0.8*pi*1i),0.8*exp(-0.8*pi*1i)), pole=c(0), gain=1)",
                                "Zpg(zero=c(1/0.9*exp(0.6*pi*1i),1/0.9*exp(-0.6*pi*1i),1/0.8*exp(0.8*pi*1i),1/0.8*exp(-0.8*pi*1i)), pole=c(0), gain=1)",
                                "N=41;M=20;hz=matrix(data=0,nrow=1,ncol=M);zw=seq(1,(M-1),by=2);hz[seq(1,(M-1),by=2)]=2/ (pi*zw);hd=c(-pracma::fliplr(as.matrix(hz)),0,hz);w=signal::hamming(N);hd*w",
                                "t=seq(0,2-1/512,by=1/512);fs=512;Ts=1/fs;N=23;M=(N-1)/2;n=1:M;h=cos(n*pi)/(Ts*n);h=c(-pracma::fliplr(as.matrix(t(h))),0,h);win=signal::hamming(N);win*h",
                                "fs=512;Ts=1/fs;N1=23;M=(N1-1)/2;n=0:(M-1);k=M-n;k2=k^2;fc=0.3*pi;h1=sin(k*fc);h2=(fc*k)*cos(k*fc);hd=(h1-h2)/(Ts*pi*k2);hd=c(hd,0,-pracma::fliplr(as.matrix(t(hd))));win=signal::hamming(N1);win*hd",
                                "butter(n=5,W=0.3,type=\"low\")",
                                "butter(n=5,W=0.65,type=\"high\")",
                                "butter(n=5,W=c(0.3,0.65),type=\"pass\")",
                                "butter(n=5,W=c(0.3,0.65),type=\"stop\")",
                                "cheby2(n=5,Rp=20,W=0.3,type=\"low\")",
                                "cheby2(n=5,Rp=20,W=0.65,type=\"high\")",
                                "cheby1(n=5,Rp=3,W=0.3,type=\"low\")", # this 48th-slot seems to be the first to always be "randomly" chosen with any new freshly-started RStudio process
                                "cheby2(n=5,Rp=20,W=c(0.3,0.65),type=\"pass\")",
                                "cheby2(n=5,Rp=20,W=c(0.3,0.65),type=\"stop\")",
                                "ellip(n=5,Rp=3,Rs=40,W=0.3,type=\"low\")",
                                "ellip(n=5,Rp=3,Rs=40,W=0.65,type=\"high\")",
                                "ellip(n=5,Rp=3,Rs=40,W=c(0.3,0.65),type=\"pass\")",
                                "ellip(n=5,Rp=3,Rs=40,W=c(0.3,0.65),type=\"stop\")",
                                "butter(n=buttord(Wp=0.285, Ws=0.345, Rp=0.5, Rs=29))",
                                "cheby1(n=cheb1ord(Wp=0.3, Ws=0.34, Rp=0.5, Rs=29))",
                                "ellip(n=ellipord(Wp=0.3, Ws=0.34, Rp=0.5, Rs=29))",
                                "omegac=0.3;zeta=0.7;C=1/(tan(pi*omegac/2));b0=1/(1+2*zeta*C+C^2);Arma(b=c(b0,2*b0,b0),a=c(1,2*b0*(1-C^2),b0*(1-2*zeta*C+C^2)))",
                                # "L=30;N=L-1;f_type=1;f_para=0.6;w_type=2;M=(N-1)/2;n=0:(M-1);if (f_type==0) {omec1=0;omec2=pi;hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='all-pass';fparamstr='[0,+infty]'} else if (f_type==1) {omec=pi*f_para;hd=sin((n-M)*omec)/((n-M)*pi);hd=c(hd,omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='LPF';fparamstr=paste0('[0, ',toString(f_para[1]),'pi]')} else if (f_type==2) {omec=pi*f_para;hd=-sin((n-M)*omec)/((n-M)*pi);hd=c(hd,1-omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='HPF';fparamstr=paste0('[',toString(f_para[1]),'pi, +infty]')} else if (f_type==3) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BPF';fparamstr=paste0('[',toString(f_para[1]),'pi, ',toString(f_para[2]),'pi]')} else if (f_type==4) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec1)-sin((n-M)*omec2))/((n-M)*pi);hd=c(hd,1-(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BSF';fparamstr=paste0('[',toString(f_para[1]),'pi,',toString(f_para[2]),'pi]')};if (w_type==0) {w=matrix(data=1,nrow=1,ncol=N);w_type_name='(rect)'} else if (w_type==1) {w=0.5*(1-cos(pi*n/M));w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hann'} else if (w_type == 2) {w=0.54-0.46*cos(pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hamm'} else if (w_type==3) {w=0.42-0.5*cos(pi*n/M)+0.08*cos(2*pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Blckmn'} else if (w_type==4) {if (is.na(b)) {b=5};w=besselI(nu=0, x=b*sqrt(1-(n/M-1)^2))/besselI(nu=0,x=b);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name=paste0('Kaiser,beta=',toString(b))};Ma(b=w*hd)",
                                # "L=70;N=L-1;f_type=3;f_para=c(0.35,0.65);w_type=4;b=5;M=(N-1)/2;n=0:(M-1);if (f_type==0) {omec1=0;omec2=pi;hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='all-pass';fparamstr='[0,+infty]'} else if (f_type==1) {omec=pi*f_para;hd=sin((n-M)*omec)/((n-M)*pi);hd=c(hd,omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='LPF';fparamstr=paste0('[0, ',toString(f_para[1]),'pi]')} else if (f_type==2) {omec=pi*f_para;hd=-sin((n-M)*omec)/((n-M)*pi);hd=c(hd,1-omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='HPF';fparamstr=paste0('[',toString(f_para[1]),'pi, +infty]')} else if (f_type==3) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BPF';fparamstr=paste0('[',toString(f_para[1]),'pi, ',toString(f_para[2]),'pi]')} else if (f_type==4) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec1)-sin((n-M)*omec2))/((n-M)*pi);hd=c(hd,1-(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BSF';fparamstr=paste0('[',toString(f_para[1]),'pi,',toString(f_para[2]),'pi]')};if (w_type==0) {w=matrix(data=1,nrow=1,ncol=N);w_type_name='(rect)'} else if (w_type==1) {w=0.5*(1-cos(pi*n/M));w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hann'} else if (w_type == 2) {w=0.54-0.46*cos(pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hamm'} else if (w_type==3) {w=0.42-0.5*cos(pi*n/M)+0.08*cos(2*pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Blckmn'} else if (w_type==4) {if (is.na(b)) {b=5};w=besselI(nu=0, x=b*sqrt(1-(n/M-1)^2))/besselI(nu=0,x=b);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name=paste0('Kaiser,beta=',toString(b))};Ma(b=w*hd)",
                                # "L=72;N=L-1;f_type=4;f_para=c(0.3,0.7);w_type=4;b=5;M=(N-1)/2;n=0:(M-1);if (f_type==0) {omec1=0;omec2=pi;hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='all-pass';fparamstr='[0,+infty]'} else if (f_type==1) {omec=pi*f_para;hd=sin((n-M)*omec)/((n-M)*pi);hd=c(hd,omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='LPF';fparamstr=paste0('[0, ',toString(f_para[1]),'pi]')} else if (f_type==2) {omec=pi*f_para;hd=-sin((n-M)*omec)/((n-M)*pi);hd=c(hd,1-omec/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='HPF';fparamstr=paste0('[',toString(f_para[1]),'pi, +infty]')} else if (f_type==3) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec2)-sin((n-M)*omec1))/((n-M)*pi);hd=c(hd,(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BPF';fparamstr=paste0('[',toString(f_para[1]),'pi, ',toString(f_para[2]),'pi]')} else if (f_type==4) {omec1=pi*f_para[1];omec2=pi*f_para[2];hd=(sin((n-M)*omec1)-sin((n-M)*omec2))/((n-M)*pi);hd=c(hd,1-(omec2-omec1)/pi,pracma::fliplr(as.matrix(t(hd))));f_type_name='BSF';fparamstr=paste0('[',toString(f_para[1]),'pi,',toString(f_para[2]),'pi]')};if (w_type==0) {w=matrix(data=1,nrow=1,ncol=N);w_type_name='(rect)'} else if (w_type==1) {w=0.5*(1-cos(pi*n/M));w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hann'} else if (w_type == 2) {w=0.54-0.46*cos(pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Hamm'} else if (w_type==3) {w=0.42-0.5*cos(pi*n/M)+0.08*cos(2*pi*n/M);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name='Blckmn'} else if (w_type==4) {if (is.na(b)) {b=5};w=besselI(nu=0, x=b*sqrt(1-(n/M-1)^2))/besselI(nu=0,x=b);w=c(w,1,pracma::fliplr(as.matrix(t(w))));w_type_name=paste0('Kaiser,beta=',toString(b))};Ma(b=w*hd)",
                                "omegac=0.2;omegacprime=tan(omegac*pi/2);cc=(omegacprime-1)/(omegacprime+1);Zpg(zero=c(-1/cc), pole=c(-cc), gain=cc)",
                                "omegac=0.2;omegacprime=tan(omegac*pi/2);cc=(omegacprime-1)/(omegacprime+1);Zpg(zero=c(-1), pole=c(-cc), gain=cc/2)",
                                "omegac=0.2;omegacprime=tan(omegac*pi/2);cc=(omegacprime-1)/(omegacprime+1);Zpg(zero=c(1), pole=c(-cc), gain=cc/2)",
                                "omegac=0.2;omegab=0.022;omegabprime=tan(omegab*pi);cc=(omegabprime-1)/(omegabprime+1);dd=-cos(pi*omegac);bvectr=c(-cc,dd*(1-cc),1);Arma(b=bvectr,a=rev(bvectr))",
                                "omegac=0.2;omegab=0.022;omegabprime=tan(omegab*pi);cc=(omegabprime-1)/(omegabprime+1);dd=-cos(pi*omegac);Zpg(zero=c(1,-1),pole=c((-dd*(1-cc)+sqrt(dd^2*(1-cc)^2+4*cc+0i))/2,(-dd*(1-cc)-sqrt(dd^2*(1-cc)^2+4*cc+0i))/2),gain=(1+cc)/2)",
                                "omegac=0.2;omegab=0.022;omegabprime=tan(omegab*pi);cc=(omegabprime-1)/(omegabprime+1);dd=-cos(pi*omegac);Zpg(zero=c(-dd+sqrt(dd^2-1+0i),-dd-sqrt(dd^2-1+0i)),pole=c((-dd*(1-cc)+sqrt(dd^2*(1-cc)^2+4*cc+0i))/2,(-dd*(1-cc)-sqrt(dd^2*(1-cc)^2+4*cc+0i))/2),gain=(1-cc)/2)",
                                "omegac=0.3;K=tan(pi*omegac/2);b0=K^2/(1+sqrt(2)*K+K^2);a1=2*(K^2-1)/(1+sqrt(2)*K+K^2);a2=(1-sqrt(2)*K+K^2)/(1+sqrt(2)*K+K^2);Arma(b=c(b0,2*b0,b0),a=c(1,a1,a2))",
                                "omegac=0.7;K=tan(pi*omegac/2);b0=1/(1+sqrt(2)*K+K^2);a1=2*(K^2-1)/(1+sqrt(2)*K+K^2);a2=(1-sqrt(2)*K+K^2)/(1+sqrt(2)*K+K^2);Arma(b=c(b0,-2*b0,b0),a=c(1,a1,a2))",
                                "omegac=0.3;G=15; K=tan(pi*omegac/2);boostV0=10^(G/20); b0=(1+sqrt(2*boostV0)*K+boostV0*K^2)/(1+sqrt(2)*K+K^2); b1=2*(boostV0*K^2-1)/(1+sqrt(2)*K+K^2); b2=(1-sqrt(2*boostV0)*K+boostV0*K^2)/(1+sqrt(2)*K+K^2); a1=2*(K^2-1)/(1+sqrt(2)*K+K^2); a2=(1-sqrt(2)*K+K^2)/(1+sqrt(2)*K+K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                                "omegac=0.3;G=-15; K=tan(pi*omegac/2);cutV0  =10^(-G/20); b0=(1+sqrt(2)*K+K^2)/(1+sqrt(2*cutV0)*K+cutV0*K^2); b1=2*(K^2-1)/(1+sqrt(2*cutV0)*K+cutV0*K^2); b2=(1-sqrt(2)*K+K^2)/(1+sqrt(2*cutV0)*K+cutV0*K^2); a1=2*(cutV0*K^2-1)/(1+sqrt(2*cutV0)*K+cutV0*K^2); a2=(1-sqrt(2*cutV0)*K+cutV0*K^2)/(1+sqrt(2*cutV0)*K+cutV0*K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                                "omegac=0.7;G=15; K=tan(pi*omegac/2);boostV0=10^(G/20); b0=(boostV0+sqrt(2*boostV0)*K+K^2)/(1+sqrt(2)*K+K^2); b1=2*(K^2-boostV0)/(1+sqrt(2)*K+K^2); b2=(boostV0-sqrt(2*boostV0)*K+K^2)/(1+sqrt(2)*K+K^2); a1=2*(K^2-1)/(1+sqrt(2)*K+K^2); a2=(1-sqrt(2)*K+K^2)/(1+sqrt(2)*K+K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                                "omegac=0.7;G=-15; K=tan(pi*omegac/2);cutV0  =10^(-G/20); b0=(1+sqrt(2)*K+K^2) /(cutV0+sqrt(2*cutV0)*K+cutV0*K^2); b1=2*(K^2-1)/(cutV0+sqrt(2*cutV0)*K+cutV0*K^2); b2=(1-sqrt(2)*K+K^2)/(cutV0+sqrt(2*cutV0)*K+cutV0*K^2); a1=2*(K^2/cutV0-1)/(1+sqrt(2/cutV0)*K+K^2/cutV0); a2=(1-sqrt(2/cutV0)*K+K^2/cutV0)/(1+sqrt(2/cutV0)*K+K^2/cutV0); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                                "omegac=0.3;Q=1.25;G=15; K=tan(pi*omegac/2);boostV0=10^(G/20); b0=(1+(boostV0/Q)*K+K^2)/(1+(1/Q)*K+K^2); b1=2*(K^2-1)/(1+(1/Q)*K+K^2); b2=(1-(boostV0/Q)*K+K^2)/(1+(1/Q)*K+K^2); a1=b1; a2=(1-(1/Q)*K+K^2)/(1+(1/Q)*K+K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                                "omegac=0.3;Q=1.25;G=-15; K=tan(pi*omegac/2);cutV0  =10^(-G/20); b0=(1+(1/Q)*K+K^2)/(1+(cutV0/Q)*K+K^2); b1=2*(K^2-1)/(1+(cutV0/Q)*K+K^2); b2=(1-(1/Q)*K+K^2)/(1+(cutV0/Q)*K+K^2); a1=b1; a2=(1-(cutV0/Q)*K+K^2)/(1+(cutV0/Q)*K+K^2); Arma(b=c(b0,b1,b2),a=c(1,a1,a2))",
                                "L=16;remez(n=L-1, f= c(0, 0.3, 0.4, 1), a= c(1,1, 0,0), ftype= \"bandpass\")",
                                "firstHalfCoef=c(0.034025544,0.006219216,-0.005305575,0.006128687,-0.005593423,0.00624262,-0.006848848,0.008979105,-0.008978654,0.017501073,-0.006953636,0.039774499,-0.064655981,0.085240952,-0.131292156);Ma(b=c(firstHalfCoef,0.195140968,rev(firstHalfCoef)))",
                                "firstHalfCoef=c(0.00012511398639,0.00001335284427,0.00016015250121,0.00000634686622,0.00026201837991,0.00007281852105,0.00045629795460,0.00022819555936,0.00071588589103,0.00047316021190,0.00110189764986,0.00088563032407,0.00184819117706,0.00188050116629,0.00282392666400,0.00363976768981,0.00591155524557,0.00644303257612,0.01406190034797,0.00537162176461,0.03594691432517,0.06164502638211,0.08276620944465,0.13009560635626);Ma(b=c(firstHalfCoef,0.19452719610477,rev(firstHalfCoef)))",
                                "firstHalfZeros=c(-1.27851808211318+0.62684824819101i,-1.36614299238589+0.27717114439577i,-1.07284006705681+0.91235620647926i,-0.77267104757162+1.12541933041583i,-0.44961436355736+1.22363585379352i,-0.20330452525073+1.29600355151311i,0.14750702212683+1.34841369533889i,0.50634734941036+1.25979827310114i,0.83329614995319+1.04982507756824i,1.04560310563666+0.67293837999034i,1.20771535897979+0.24059979456907i,0.95139216401989+0.56315210274529i,0.77837030003487+0.46073626392613i,0.79640192035334+0.15865836018951i,0.67627030631208+0.43523995090672i,0.46384030886663+0.58436750039291i,0.27466939387352+0.68338074343342i,0.08016782642482+0.73284236586261i,-0.11813451163119+0.75307102211071i,-0.26456653442537+0.72002392155667i,-0.41461446573694+0.60389882069075i,-0.70304843505842+0.14263861132902i,-0.63057376627246+0.30916579614562i,-0.54091513103870+0.46000078868751i);Zpg(zero=c(firstHalfZeros,Conj(firstHalfZeros)),pole=c(0),gain=0.00012511398639)",
                                "Zpg(zero=c(0.9049098+0.1414979i,0.9049098-0.1414979i,1.192327), pole=c(0.9588639+0.7240575i,0.9588639-0.7240575i,1.511628), gain=1)",
                                "p=3;n=2;sgolay(p,(2*n+1)) %*% c(1,rep(0,times=(2*n+1)-1))",
                                "Arma(b=c(0.049922035,-0.095993537,0.050612699,-0.004408786),a=c(1,-2.494956002,2.017265875,-0.522189400))",
                                "Zpg(pole=c(0.99572754,0.94790649,0.53567505),zero=c(0.98443604,0.83392334,0.07568359),gain=1)",
                                "Arma(b=c(1,-0.939137),a=c(1,-0.1,-0.8))",
                                "Ma(b=c(-0.01877334400452212,0.0030135586664219974,0.032677234546254562,0.047093583340252425,0.026549517702290821,-0.027522224030017681,-0.085224875017083229,-0.099447435991926181,-0.03214726739831468,0.11903714817925418,0.31117641155547854,0.47200389556543276,0.53463207017815584,0.47200389556543276,0.31117641155547854,0.11903714817925418,-0.03214726739831468,-0.099447435991926181,-0.085224875017083229,-0.027522224030017681,0.026549517702290821,0.047093583340252425,0.032677234546254562,0.0030135586664219974,-0.01877334400452212))",
                                "D=2.4;N=3;a=rep(1,times=N);for (k in (0:N)) {a[k+1]=(-1)^k*choose(N,k)*{accu=1;for (i in 0:N) {accu=accu*(((D-N+i)/(D-N+k+i)))};accu}};Arma(b=rev(a),a=a)", # "Zpg(zero=c(6.59457337747154+10.2534695974275i,6.59457337747154-10.2534695974275i,-1.61771818351450),pole=c(-0.618154639164343,0.0443714372292301+0.0689902373636964i,0.0443714372292301-0.0689902373636964i),gain=0.00415923945335710)",
                                "chebwin(n=50, at=100)",
                                "kaiser(n=101, beta=0)",
                                "kaiser(n=101, beta=50)",
                                "with(kaiserord(f=c(0.275,0.325), m=c(1,0), dev=c(0.1,0.1)),fir1(n=n,w=Wc,type=type,window=kaiser(n+1,beta),scale=FALSE))",
                                "Ma(b=rep(1,times=101))",
                                "bartlett(41)",
                                "blackman(41)",
                                "boxcar(41)",
                                "flattopwin(41,sym=\"symmetric\")",
                                "flattopwin(41,sym=\"periodic\")",
                                "sd=0.2;gausswin(41,w=1/sd)",
                                "hanning(41)",
                                "hamming(41)",
                                "triang(41)",
                                "N=19;leftside=sin(pi*(0.3*(-(N/2):(-1))))/(pi*(0.3*(-(N/2):(-1))));c(leftside,1,rev(leftside))*hamming(N+1)", # "N=18;sinc(0.3*(-(N/2):(N/2)))*0.3*blackman(N+1)",
                                "spencerFilter()",
                                "Ma(b=c(-3, -6, -5, 3, 21, 46, 67, 74, 67, 46, 21, 3, -5, -6, -3) / 320)"
                              ),
                              1
                            ),
                            multiple = FALSE,
                            options = list(create = TRUE,
                                           placeholder = "type here, or pull-down for options")
                          ),
                          tags$span(
                            title = "tooltip: equivalent R-command to above ('signal' package)",
                            align = "center",
                            verbatimTextOutput(outputId = "filterCommand")
                          )
                        )
                      ),
                      tabPanel(
                        style = paste0("background-color: ", SubTabPanelBackgroundColor,
                                       ";"),
                        title = HTML("Import &cudarrr;"),
                        value = "Import",
                        align = "center",
                        tags$span(
                          title = "tooltip: Coordinates-Import Tab of Sidebar",
                          # http://stackoverflow.com/questions/19686581/make-conditionalpanel-depend-on-files-uploaded-with-fileinput
                          fileInput(
                            inputId = "filecoordsImport",
                            label = "Import some Pole/Zero Coordinates",
                            multiple = FALSE,
                            accept = c("text/csv", ".csv",
                                       ".txt", ".RData", ".mat", ".bin", ".yml")
                          ),
                          tags$hr(),
                          radioButtons(
                            inputId = "textbinaryformatImport",
                            label = "Choose file-format of import",
                            choices = c(
                              `Text/CSV` = "csv",
                              `C-style binary` = "bin",
                              `.RData (R-style binary)` = "RData",
                              `MATLAB-binary (.mat)` = "mat",
                              YAML = "yml"
                            ),
                            selected = "csv",
                            inline = TRUE
                          ),
                          conditionalPanel(
                            condition = "input.textbinaryformatImport=='csv'",
                            checkboxInput(
                              inputId = "coordsevaluateImport",
                              label = "Evaluate input-lines as commands?",
                              value = TRUE
                            )
                          ),
                          tags$hr(),
                          helpText("verbatimTextOutput2import"),
                          verbatimTextOutput("coordsfilecontentsPrint2Import")
                        )
                      ),
                      tabPanel(
                        style = paste0("background-color: ", SubTabPanelBackgroundColor,
                                       ";"),
                        title = HTML("Export &nearhk;"),
                        value = "Export",
                        align = "center",
                        tags$span(
                          title = "tooltip: Coordinates-Export Tab of Sidebar",
                          textInput(
                            inputId = "filenameExport",
                            label = "Filename to write to, but without extension (overwrites)",
                            value = "poleszeroslocs",
                            placeholder = "Enter filename here"
                          ),
                          checkboxInput(
                            inputId = "IncludeTimeStampExport",
                            label = "Include a Time-Stamp as part of Export's filename?",
                            value = TRUE
                          ),
                          tags$hr(),
                          radioButtons(
                            inputId = "textbinaryformatExport",
                            label = "Choose file-format for export",
                            choices = c(
                              `CSV (textual)` = "csv",
                              `C-style binary` = "bin",
                              `.RData (R-style binary)` = "RData",
                              `MATLAB-binary (.mat)` = "mat",
                              YAML = "yml"
                            ),
                            selected = "csv",
                            inline = TRUE
                          ),
                          conditionalPanel(
                            condition = "input.textbinaryformatExport=='csv'",
                            checkboxInput(
                              inputId = "coordsheaderExport",
                              label = "Add a comment as first-line of file?",
                              value = TRUE
                            ),
                            conditionalPanel(
                              condition = "input.coordsheaderExport", # Boolean TRUE/FALSE
                              textInput(
                                inputId = "headerlineExport",
                                label = "Header-Line:",
                                value = "poleszeroslocs",
                                placeholder = "Enter header-line here"
                              )
                            ),
                            checkboxInput(
                              inputId = "coordsevaluateExport",
                              label = "Store as commands?",
                              value = TRUE
                            )
                          )
                        ),
                        br(),
                        downloadButton(outputId = "downloadCoordsExport",
                                       label = "Export Pole/Zero Coords (choose file-format above)"),
                        tags$hr(),
                        helpText("verbatimTextOutput2export"),
                        verbatimTextOutput(outputId = "coordsfilecontentsPrint2export")
                      )
                    ),
                    tags$span(
                      title = "tooltip: poles/zeros location-editing",
                      fluidRow(
                        tags$span(
                          title = "tooltip: currently-selected pole/ zero\nfor editing points, clear this field to use above pull-down controls instead",
                          textInput(
                            inputId = "edit_currentSelectionText",
                            label = NULL,
                            value = ""
                          )
                        )
                      ),
                      shinyjs::hidden(fluidRow(
                        column(
                          width = 6L,
                          uiOutput(outputId = "slider2AWidget"),
                          uiOutput(outputId = "stretchyslider2ArangeWidget"),
                          uiOutput(outputId = "stretchyslider2AstepWidget"),
                          uiOutput(outputId = "slider2AanimintervalWidget")
                        ),
                        column(
                          width = 6L,
                          uiOutput(outputId = "slider2BWidget"),
                          uiOutput(outputId = "stretchyslider2BrangeWidget"),
                          uiOutput(outputId = "stretchyslider2BstepWidget"),
                          uiOutput(outputId = "slider2BanimintervalWidget")
                        )
                      )),
                      fluidRow(
                        column(
                          width = 6L,
                          align = "center",
                          fluidRow(
                            tags$span(
                              title = "tooltip: Add the above as a new zero-point/ conjugate-pair",
                              shinyBS::bsButton(
                                inputId = "pb_addzero",
                                label = "Add O",
                                width = "31.5%",
                                style = "primary",
                                size = "small",
                                type = "action",
                                block = FALSE,
                                disabled = FALSE,
                                icon = shiny::icon("circle-o", lib = "font-awesome")
                              )
                            ),
                            tags$span(
                              title = "tooltip: Replace currently-selected zero-point below (unlinks pair) with above",
                              shinyBS::bsButton(
                                inputId = "pb_editzero",
                                label = "Edit O",
                                width = "31.5%",
                                style = "default",
                                size = "small",
                                type = "action",
                                block = FALSE,
                                disabled = FALSE,
                                icon = shiny::icon("pencil-square-o",
                                                   lib = "font-awesome")
                              )
                            ),
                            tags$span(
                              title = "tooltip: Delete the currently-selected zero-point/ conjugate-pair below",
                              shinyBS::bsButton(
                                inputId = "pb_delzero",
                                label = "Del O",
                                width = "31.5%",
                                style = "warning",
                                size = "small",
                                type = "action",
                                block = FALSE,
                                disabled = FALSE,
                                icon = shiny::icon("minus-circle", lib = "font-awesome")
                              )
                            )
                          ),
                          fluidRow(
                            tags$span(
                              title = "tooltip: List of current zero-points",
                              selectInput(
                                inputId = "listbox_zero",
                                label = HTML("&cir; Zero Locations O"),
                                choices = c("0"),
                                selectize = FALSE,
                                size = 10L
                              )
                            )
                          )
                        ),
                        column(
                          width = 6L,
                          align = "center",
                          fluidRow(
                            tags$span(
                              title = "tooltip: Add the above as a new pole-point/ conjugate-pair",
                              shinyBS::bsButton(
                                inputId = "pb_addpole",
                                label = "Add X",
                                width = "31.5%",
                                style = "primary",
                                size = "small",
                                type = "action",
                                block = FALSE,
                                disabled = FALSE,
                                icon = shiny::icon("times", lib = "font-awesome")
                              )
                            ),
                            tags$span(
                              title = "tooltip: Replace currently-selected pole-point below (unlinks pair) with above",
                              shinyBS::bsButton(
                                inputId = "pb_editpole",
                                label = "Edit X",
                                width = "31.5%",
                                style = "default",
                                size = "small",
                                type = "action",
                                block = FALSE,
                                disabled = FALSE,
                                icon = shiny::icon("pencil-square-o",
                                                   lib = "font-awesome")
                              )
                            ),
                            tags$span(
                              title = "tooltip: Delete the currently-selected pole-point/ conjugate-pair below",
                              shinyBS::bsButton(
                                inputId = "pb_delpole",
                                label = "Del X",
                                width = "31.5%",
                                style = "warning",
                                size = "small",
                                type = "action",
                                block = FALSE,
                                disabled = FALSE,
                                icon = shiny::icon("minus-square",
                                                   lib = "font-awesome")
                              )
                            )
                          ),
                          fluidRow(
                            tags$span(
                              title = "tooltip: List of current pole-points",
                              selectInput(
                                inputId = "listbox_pole",
                                label = HTML("&times; Pole Locations X"),
                                choices = c("0"),
                                selectize = FALSE,
                                size = 10L
                              )
                            )
                          )
                        )
                      )
                    ),
                    bsAlert(anchorId = "AlertMsgToUser")
                  )
                )
              ) # end sidePanel
              ,data.step=1, 
              data.intro="This is the (Left/ Input) Side-panel.", 
              data.hint="Hint: This is the (Left/ Input) Side-panel."
              ), # end rintrojs::introBox
              # .. mainPanel ----
              rintrojs::introBox(
              mainPanel(
                id = "mainPanel",
                style = paste0("background-color: ", MainSidePanelBackgroundColor,
                               ";"),
                width = 12L - mySidebarWidth,
                shinyBS::bsCollapse(
                  id = "mainPanelCollapse",
                  multiple = TRUE,
                  open = "mainPanelCollapse1",
                  shinyBS::bsCollapsePanel(
                    value = "mainPanelCollapse1",
                    title = "Results Panel (click to expand/ collapse):",
                    style = "info",
                    tabsetPanel(
                      type = "tabs",
                      id = "ResultsPanel",
                      selected = "Magnitude",
                      tabPanel(
                        style = paste0("background-color: ",
                                       TabPanelBackgroundColor, ";"),
                        title = "Zoom",
                        value = "Zoom",
                        icon = shiny::icon("search",
                                           lib = "font-awesome"),
                        tags$span(
                          title = "tooltip: Zoom-In Tab of Plots Page\n(use browser's right-mouse-click/ context-menu for image download-options)",
                          if ((scalePlotsToVerticalHeight)) {
                            tags$head(tags$style(
                              paste0(
                                "#axes_pzplotZoom{height:",
                                verticalHeightOfPlots,
                                " !important;}"
                              )
                            ))
                          },
                          plotOutput(
                            outputId = "axes_pzplotZoom",
                            width = "100%",
                            height = "750px",
                            inline = FALSE
                          )
                        )
                      ),
                      tabPanel(
                        style = paste0("background-color: ", TabPanelBackgroundColor,
                                       ";"),
                        title = HTML("Mag |H(e<sup> &jmath;&omega;</sup>)|"),
                        value = "Magnitude",
                        icon = shiny::icon("area-chart",
                                           lib = "font-awesome"),
                        if ((scalePlotsToVerticalHeight)) {
                          tags$head(tags$style(
                            paste0("#axes_mag{height:",
                                   verticalHeightOfPlots, " !important;}")
                          ))
                        },
                        if ((scalePlotsToVerticalHeight)) {
                          tags$head(tags$style(
                            paste0(
                              "#axes_magpassbandstopband{height:",
                              verticalHeightOfPlots,
                              " !important;}"
                            )
                          ))
                        },
                        tags$span(
                          title = "tooltip: Magnitude-Response Tab of Plots Page\n(use browser's right-mouse-click/ context-menu for image download-options)",
                          plotOutput(
                            outputId = "axes_mag",
                            width = "100%",
                            height = "650px",
                            inline = FALSE,
                            hover = hoverOpts(
                              id = "magplot_hover",
                              delay = 200,
                              delayType = "throttle"
                            )
                          ),
                          uiOutput(outputId = "hover_info2"),
                          plotOutput(
                            outputId = "axes_magpassbandstopband",
                            width = "100%",
                            height = "650px",
                            inline = FALSE
                          ),
                          tags$div(
                            title = "tooltip: The chosen-range, [min, max], can be dragged together, as one piece",
                            column(
                              width = 6L,
                              align = "center",
                              uiOutput(outputId = "zoomlimXpassbandWidget"),
                              uiOutput(outputId = "zoomlimYpassbandWidget")
                            ),
                            column(
                              width = 6L,
                              align = "center",
                              uiOutput(outputId = "zoomlimXstopbandWidget"),
                              uiOutput(outputId = "zoomlimYstopbandWidget")
                            )
                          )
                        ),
                        hr(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        shinyBS::bsCollapse(
                          id = "magPanelCollapse",
                          shinyBS::bsCollapsePanel(
                            value = "magPanelCollapse1",
                            title = "Beamformer Array Panel (click to expand/ collapse):",
                            style = "info",
                            if ((scalePlotsToVerticalHeight)) {
                              tags$head(tags$style(
                                paste0(
                                  "#axes_beamformer{height:",
                                  verticalHeightOfPlots,
                                  " !important;}"
                                )
                              ))
                            },
                            tags$span(
                              title = "tooltip: Polar-Plot of Pattern-Loss for Multi-Element Array Beamformer pattern, half-wavelength spacing\n(use browser's right-mouse-click/ context-menu for image download-options)",
                              plotOutput(
                                outputId = "axes_beamformer",
                                width = "100%",
                                height = "650px",
                                inline = FALSE
                              )
                            )
                          )
                        )
                      ),
                      tabPanel(
                        style = paste0("background-color: ", TabPanelBackgroundColor,
                                       ";"),
                        title = HTML("Ph &varphi;=&angle;H(e<sup>&jmath;&omega;</sup>)"),
                        icon = shiny::icon("line-chart", lib = "font-awesome"),
                        tags$span(
                          title = "tooltip: Phase-Response Tab of Plots Page\n(use browser's right-mouse-click/ context-menu for image download-options)",
                          if ((scalePlotsToVerticalHeight)) {
                            tags$head(tags$style(
                              paste0(
                                "#axes_phase{height:",
                                verticalHeightOfPlots,
                                " !important;}"
                              )
                            ))
                          },
                          plotOutput(
                            outputId = "axes_phase",
                            width = "100%",
                            height = "650px",
                            inline = FALSE
                          )
                        ),
                        tags$span(
                          title = "tooltip: Group-Delay\n(use browser's right-mouse-click/ context-menu for image download-options)",
                          if ((scalePlotsToVerticalHeight)) {
                            tags$head(tags$style(
                              paste0(
                                "#axes_grpdelay{height:",
                                verticalHeightOfPlots,
                                " !important;}"
                              )
                            ))
                          },
                          plotOutput(
                            outputId = "axes_grpdelay",
                            width = "100%",
                            height = "650px",
                            inline = FALSE
                          )
                        )
                      ),
                      tabPanel(
                        style = paste0("background-color: ",
                                       TabPanelBackgroundColor, ";"),
                        title = HTML("Imp h<sub>[n]</sub>"),
                        icon = shiny::icon("signal", lib = "font-awesome"),
                        tags$span(
                          title = "tooltip: Impulse-Response Tab of Plots Page\n(use browser's right-mouse-click/ context-menu for image download-options)",
                          if ((scalePlotsToVerticalHeight)) {
                            tags$head(tags$style(
                              paste0("#axes_imp{height:",
                                     verticalHeightOfPlots, " !important;}")
                            ))
                          },
                          plotOutput(
                            outputId = "axes_imp",
                            width = "100%",
                            height = "650px",
                            inline = FALSE
                          ),
                          uiOutput(outputId = "system_stable",
                                   inline = TRUE),
                          uiOutput(outputId = "system_real",
                                   inline = TRUE),
                          br(),
                          br(),
                          tags$div(
                            align = "right",
                            numericInput(
                              inputId = "maxLengthImpulseResponse",
                              label = "max-length of Imp-Response",
                              value = 25L,
                              min = 2L,
                              max = 1001L,
                              step = 1L
                            )
                          ),
                          hr(),
                          tags$span(
                            title = "tooltip: Unit-Step Response.",
                            shinyBS::bsCollapse(
                              id = "stepPlotCollapse",
                              multiple = TRUE,
                              shinyBS::bsCollapsePanel(
                                value = "stepCollapse",
                                title = "Unit-Step Response (click to expand/ collapse):",
                                style = "info",
                                if ((scalePlotsToVerticalHeight)) {
                                  tags$head(tags$style(
                                    paste0("#axes_step{height:",
                                           verticalHeightOfPlots, " !important;}")
                                  ))
                                },
                                plotOutput(
                                  outputId = "axes_step",
                                  width = "100%",
                                  height = "500px",
                                  inline = FALSE
                                )
                              )
                            )
                          ),
                          hr(),
                          tags$span(
                            title = "tooltip: In the estimation of a moving-average/ MA model, the ACF can be used to determine appropriate number of lagged error-terms that need to be included;\nPACF can help determine the appropriate lags, p, in an auto-regressive (recursive) AR(p) model, or in an extended ARIMA(p,d,q) model;\nThe confidence-interval (blue dashed-line) is based upon an uncorrelated-series, and should be treated with caution.",
                            shinyBS::bsCollapse(
                              id = "acfPlotsCollapse",
                              multiple = TRUE,
                              shinyBS::bsCollapsePanel(
                                value = "acfCollapse",
                                title = "Auto-Correlation Function (click to expand/ collapse):",
                                style = "info",
                                if ((scalePlotsToVerticalHeight)) {
                                  tags$head(tags$style(
                                    paste0("#axes_acf{height:",
                                           verticalHeightOfPlots, " !important;}")
                                  ))
                                },
                                plotOutput(
                                  outputId = "axes_acf",
                                  width = "100%",
                                  height = "500px",
                                  inline = FALSE
                                )
                              )
                            )
                          )
                        )
                      ),
                      shinyjs::hidden(
                        tabPanel(
                          style = paste0("background-color: ",
                                         TabPanelBackgroundColor, ";"),
                          title = "All",
                          icon = shiny::icon("list", lib = "glyphicon"),
                          tags$span(
                            title = "tooltip: Summary All-in-1L Tab of Plots Page",
                            helpText("Welcome to the Summary All-in-1L Tab..."),
                            br(),
                            tags$span(
                              title = "tooltip: display GUI-plots as separate window: Pole-Zero, Impulse-response, Magnitude, Phase",
                              actionButton(
                                width = "100%",
                                inputId = "pb_showgph",
                                label = "Graph",
                                icon = shiny::icon("object-group",
                                                   lib = "font-awesome")
                              )
                            )
                          )
                        )
                      ),
                      shinyjs::hidden(
                        tabPanel(
                          style = paste0("background-color: ",
                                         TabPanelBackgroundColor, ";"),
                          title = "Tbl",
                          icon = shiny::icon("table", lib = "font-awesome"),
                          helpText("Welcome to the Table Tab...")
                        )
                      ),
                      tabPanel(
                        style = paste0("background-color: ",
                                       TabPanelBackgroundColor, ";"),
                        title = "Report",
                        icon = shiny::icon("book", lib = "font-awesome"),
                        fluidPage(
                          fluidRow(HTML(
                            markdown::markdownToHTML(fragment.only = TRUE,
                                                     text = "This generates a downloadable report, whenever the `Download` button is clicked.  It uses an .Rmd file to generate the report. Note that this app uses a temporary-directory, because the `app`'s directory might not be writable, when eventually deployed. (http://shiny.rstudio.com/articles/generating-reports.html)")
                          )),
                          fluidRow(
                            radioButtons(inputId = "reportFormat",
                                         label = "Choose Report Format",
                                         choices = c(
                                           `HTML document`="html",
                                           `Adobe PDF document`="pdf",
                                           `PDFLaTeX`="latex",
                                           `Slidy (slides) presentation`="slidy",
                                           `MS Word document`="word",
                                           `Beamer (slides) presentation`="beamer",
                                           `ioSlides (slides) presentation`="ioslides"
                                         ),
                                         selected = "html",
                                         inline=FALSE
                                         ),
                            textAreaInput(
                              inputId = "myPrefaceNote",
                              label = "Preface Note/ Executive Summary:",
                              value = "",
                              width = "100%",
                              height = "100%",
                              cols = 80L,
                              rows = 8L,
                              placeholder = "Enter any text that you want to appear at the start of the report\n(Note: RMarkdown/ LaTeX formatting markup is acceptable)\nCan drag the corner to expand entry-area's size",
                              resize = NULL
                            ),
                            sliderInput(inputId = "slider", 
                                        label="Example Control-Input (e.g. a Slider)",
                                        min=1L, 
                                        max=100L, 
                                        value=50L
                                        ),
                            downloadButton(outputId = "report",
                                           "Generate report")
                          )
                        )
                      )
                    )
                  )
                )
              )
              ,data.step=2,
              data.intro="This is the Main (Results) panel.", 
              data.hint="Hint: This is the Main (Results) panel."
              ) # end rintrojs::introBox
            )
          )), # end FluidPage
          withMathJax(),
          rintrojs::introBox(
          shinyBS::bsCollapse(
            id = "FnBoxesCollapse",
            multiple = TRUE,
            shinyBS::bsCollapsePanel(
              value = "TrFnCollapse",
              title = "Transfer-Function, (z)-Domain (click to expand/ collapse):",
              style = "info",
              fluidRow(
                tags$span(
                  title = "tooltip: z-transform is most useful when the infinite-sum can be expressed as a simple mathematical formula. One important form of representation is to represent it as a rational-function (as shown) where the numerator and denominator are polynomials in z. The values of z for which H(z)=0L are called the zeros of H(z), and the values of z for which H(z) is infinity are referred to as the poles of H(z). The zeros are the roots of the numerator-polynomial, and the poles of H(z) (for finite-values of z) are the roots of the denominator-polynomial.  A plot of Poles and Zeros of a system on the z-plane is called a Pole-Zero plot.\n(use MathJax's right-mouse-click/ context-menu for download into your (La)TeX/ MathML (e.g. MS Word) documents and/or for zooming-options)",
                  uiOutput(
                    outputId = "transferfn",
                    inline = FALSE,
                    container = tags$span
                  )
                )
              )
            ),
            shinyBS::bsCollapsePanel(
              value = "DiffEqCollapse",
              title = "Difference-Equation, Time (n)-Domain (click to expand/ collapse):",
              style = "info",
              tags$span(title = "tooltip: Difference-equations are a discrete-time version of the continuous-time differential-equations.\n(use MathJax's right-mouse-click/ context-menu for download-/ zooming-options)",
                        wellPanel(
                          style = paste0("background-color: ", WellPanelBackgroundColor,
                                         ";"),
                          uiOutput(
                            outputId = "diffeqn",
                            inline = FALSE,
                            container = tags$span
                          )
                        )),
              wellPanel(
                style = paste0("background-color: ",
                               WellPanelBackgroundColor, ";"),
                tabsetPanel(
                  type = "tabs",
                  tabPanel(
                    title = "Coeff. (b,a,G)",
                    tags$span(
                      title = "tooltip: Coefficients Tab of Plots page",
                      tags$span(
                        title = "tooltip: Gain",
                        numericInput(
                          inputId = "edit_gain",
                          label = "Gain",
                          value = 1L,
                          # min = 0L,
                          # max = 10L,
                          step = 0.1
                        ),
                        shinyBS::bsButton(
                          inputId = "unitymax_gain",
                          label = "Make unity (=1.0) as max freq-response",
                          icon = shiny::icon("caret-square-o-up", lib = "font-awesome"),
                          style = "primary",
                          size = "small",
                          type = "toggle",
                          block = FALSE,
                          disabled = FALSE,
                          value = FALSE
                        )
                      )
                    )
                  ),
                  tabPanel(
                    title = "Form",
                    radioButtons(inputId = "filterForm",
                                         label = "Choose Filter Form",
                                         choices = c(
                                           `Direct Form I`="df1",
                                           `Direct Form II (canonical)`="df2",
                                           `DF I, transposed`="df1t",
                                           `DF II, transposed`="df2t",
                                           `Cascaded Second-Order Sections`="sos"
                                         ),
                                         selected = "df1",
                                         inline=TRUE
                                         ),
                    # uiOutput(outputId = "filterFormWidget"),
                    conditionalPanel(
                      condition = "input.filterForm=='df1'",
                      tags$div(
                        title = "tooltip: IIR digital-filter, Direct-Form I\n(use browser's right-mouse-click/ context-menu for image download-options)",
                        img(src = "DirectFormI.png", align = "right", width = "100%"
                          , alt = "IIR digital-filter, Direct-Form I"
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.filterForm=='df2'",
                      tags$div(
                        title = "tooltip: IIR digital-filter, (Canonical) Direct-Form II\n(use browser's right-mouse-click/ context-menu for image download-options)",
                        img(src = "DirectFormII.png", align = "right", width = "100%"
                          , alt = "IIR digital-filter, Direct-Form II"
                        )
                      )
                    ),
                    conditionalPanel(
                      condition = "input.filterForm=='df1t'",
                      helpText("(not implemented yet)")
                    ),
                    conditionalPanel(
                      condition = "input.filterForm=='df2t'",
                      helpText("(not implemented yet)")
                    ),
                    conditionalPanel(
                      condition = "input.filterForm=='sos'",
                      helpText("(not implemented yet)")
                    ),
                    hr()
                  )
                )
              ),
              hr(),
              tags$span(title = "tooltip: b/a filter-coefficient editing",
                        fluidRow(
                          column(
                            width = 6L,
                            align = "center",
                            shinyjs::hidden(fluidRow(
                              tags$span(
                                title = "tooltip: Add a new b-coefficient",
                                shinyBS::bsButton(
                                  inputId = "pb_addb",
                                  label = "Add",
                                  width = "31.5%",
                                  style = "primary",
                                  size = "small",
                                  type = "action",
                                  block = FALSE,
                                  disabled = TRUE,
                                  icon = shiny::icon("circle-o", lib = "font-awesome")
                                )
                              ),
                              tags$span(
                                title = "tooltip: Replace currently-selected b-coefficient below (unlinks pair) with above",
                                shinyBS::bsButton(
                                  inputId = "pb_editb",
                                  label = "Edit",
                                  width = "31.5%",
                                  style = "default",
                                  size = "small",
                                  type = "action",
                                  block = FALSE,
                                  disabled = TRUE,
                                  icon = shiny::icon("pencil-square-o", lib = "font-awesome")
                                )
                              ),
                              tags$span(
                                title = "tooltip: Delete the currently-selected b-coefficient below",
                                shinyBS::bsButton(
                                  inputId = "pb_delb",
                                  label = "Del",
                                  width = "31.5%",
                                  style = "warning",
                                  size = "small",
                                  type = "action",
                                  block = FALSE,
                                  disabled = TRUE,
                                  icon = shiny::icon("minus-circle", lib = "font-awesome")
                                )
                              )
                            )),
                            fluidRow(
                              tags$span(
                                title = "tooltip: List of current b-coefficients",
                                selectInput(
                                  inputId = "listbox_b",
                                  label = "b Coeff. (mov-avg MA, feed-forward)",
                                  choices = c("0"),
                                  selectize = FALSE,
                                  size = 10L
                                )
                              )
                            )
                          ),
                          column(
                            width = 6L,
                            align = "center",
                            shinyjs::hidden(fluidRow(
                              tags$span(
                                title = "tooltip: Add a new a-coefficient",
                                shinyBS::bsButton(
                                  inputId = "pb_adda",
                                  label = "Add",
                                  width = "31.5%",
                                  style = "primary",
                                  size = "small",
                                  type = "action",
                                  block = FALSE,
                                  disabled = TRUE,
                                  icon = shiny::icon("times", lib = "font-awesome")
                                )
                              ),
                              tags$span(
                                title = "tooltip: Replace currently-selected a-coefficient below (unlinks pair) with above",
                                shinyBS::bsButton(
                                  inputId = "pb_edita",
                                  label = "Edit",
                                  width = "31.5%",
                                  style = "default",
                                  size = "small",
                                  type = "action",
                                  block = FALSE,
                                  disabled = TRUE,
                                  icon = shiny::icon("pencil-square-o", lib = "font-awesome")
                                )
                              ),
                              tags$span(
                                title = "tooltip: Delete the currently-selected a-coefficient below",
                                shinyBS::bsButton(
                                  inputId = "pb_dela",
                                  label = "Del",
                                  width = "31.5%",
                                  style = "warning",
                                  size = "small",
                                  type = "action",
                                  block = FALSE,
                                  disabled = TRUE,
                                  icon = shiny::icon("minus-square", lib = "font-awesome")
                                )
                              )
                            )),
                            fluidRow(
                              tags$span(
                                title = "tooltip: List of current a-coefficients",
                                selectInput(
                                  inputId = "listbox_a",
                                  label = "a Coeff. (auto-regressive AR, feedback)",
                                  choices = c("0"),
                                  selectize = FALSE,
                                  size = 10L
                                )
                              )
                            )
                          )
                        ))
            ) # end shinyBS::bsCollapsePanel
          ) # end shinyBS::bsCollapse
          ,data.step=3, 
          data.intro="These are the Equation-panels.", 
          data.hint="Hint: These are the Equation-panels."
          ), # end rintrojs::introBox
          rintrojs::introBox(
          shinyBS::bsCollapse(
            id = "AudioCollapse",
            multiple = TRUE,
            shinyBS::bsCollapsePanel(
              value = "Audio1Collapse",
              title = "Audio-Filtering (click to expand/ collapse):",
              style = "info",
              fluidRow(
                tags$span(
                  title = "tooltip: Audio",
                  # shinyBS::bsButton(
                  #                 inputId = "pb_music",
                  #                 label = "Play Handel's Messiah",
                  #                 # width = "31.5%",
                  #                 style = "default", # default, primary, success, info, warning, or danger
                  #                 size = "default", # extra-small, small, default, or large
                  #                 type = "action", # action, toggle
                  #                 block = FALSE,
                  #                 disabled = FALSE,
                  #                 icon = shiny::icon("file-audio-o", lib = "font-awesome") # "play" # "music" #
                  #               ),
                  # shinyBS::bsButton(
                  #                 inputId = "pb_musicfiltered",
                  #                 label = "Play Filtered Handel's Messiah",
                  #                 # width = "31.5%",
                  #                 style = "primary", # default, primary, success, info, warning, or danger
                  #                 size = "default", # extra-small, small, default, or large
                  #                 type = "action", # action, toggle
                  #                 block = FALSE,
                  #                 disabled = FALSE,
                  #                 icon = shiny::icon("file-audio-o", lib = "font-awesome") # "play" # "music" #
                  #               ),
                  column(width=3,
                  radioButtons(
                    inputId = "inputsignalsource",
                    label = "Choose audio input",
                    choices = c(
                      `From file` = "file",
                      `Use generator` = "generator"
                    ),
                    selected = "file",
                    inline = TRUE
                  ),
                  conditionalPanel(
                    condition = "input.inputsignalsource=='file'",
                    fileInput(inputId = "filenameAudio",
                              label = HTML("Filename to play/ filter, x<sub>[n]</sub>"),
                              # value = "4ClassStream",
                              # placeholder = "Enter filename here",
                              multiple = FALSE,
                              accept = c(".wav",".mat",".mp3")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.inputsignalsource=='generator'",
                    radioButtons(
                      inputId = "audiogeneratorsignal",
                      label = "Generate signal",
                      choices = c(
                        `white-noise (mean=0, sd=1)`="whitenoise",
                        `pink-noise, 1/f`="pinknoise",
                        `pulsed, parameter1 [cycles-per-sec], width=0.2, plateau=0.95, interval=1`="pulsed",
                        `saw-tooth, parameter1 [cycles]`="sawtooth",
                        # `silence`="silence",
                        `sine-wave, frq=parameter1 [Hz]`="sinewave",
                        `square-wave, parameter1 [cycles], 50% duty`="squarewave",
                        `Heavi-Sine (0.3t,0.72t)`="heavisine",
                        `Bumps (.1t,.13t,.15t, ...)`="bumps",
                        `Blocks (.1t,.13t,.15t, ...)`="blocks",
                        `Doppler (Donoho)`="doppler",
                        `Ramp (0.37t)`="ramp",
                        `Cusp (0.37t)`="cusp",
                        `Sing (0.37t)`="sing",
                        `Hi-Sine, 0.6902(Fs/2)`="hisine",
                        `Lo-Sine, 0.3333(Fs/2)`="losine",
                        `Linear-Chirp`="linchirp",
                        `Two-Chirp`="twochirp",
                        `Quadratic-chirp`="quadchirp",
                        `Mish-Mash (Donoho)`="mishmash",
                        `Werner-sorrows (Heisenburg)`="wernersorrows",
                        `Leopold (Kronecker, 0.37t)`="leopold",
                        `Southern-Oscill. '50-'87, 453mths=37.75yrs soi (astsa)`="astsasoi",
                        `Cardiovasc.-mortality, 508wks=9.77yrs, '70-'79 (astsa)`="astsacmort",
                        `Star-magnitude, 600days=19.6mths (astsa)`="astsastar",
                        `NYSE '84-'91, 2000days=7.94yrs (21days/mth) (astsa)`="astsanyse",
                        `Sunspots, 1749-2014, 3177mths=264.75yrs`="sunspotmonthly",
                        `Simulated ARIMA(p=2,d=0,q=2) filtered-noise`="arimapdq",
                        `(custom)`="custom"
                      ),
                      selected = "whitenoise",
                      inline = TRUE
                    ),
                    conditionalPanel(
                      condition = paste0("input.audiogeneratorsignal=='pulsed' || ",
                                         "input.audiogeneratorsignal=='sawtooth' || ",
                                         "input.audiogeneratorsignal=='sinewave' || ",
                                         "input.audiogeneratorsignal=='squarewave'"),
                      numericInput(
                        inputId = "parameter1",
                        label = "Parameter1 (cycles, or Hz=cycles-per-sec)",
                        value = 3,
                        min = 0.0,
                        max = 22050.0
                        ,step = 0.5
                      )
                    ),
                    conditionalPanel(
                      condition = "input.audiogeneratorsignal=='pulsed'",
                      numericInput(
                        inputId = "parameter2",
                        label = "width (% of interval)=PW",
                        value = 0.2,
                        min = 0.0,
                        max = 1.0
                        ,step = 0.01
                      ),
                      numericInput(
                        inputId = "parameter3",
                        label = "plateau (% of width)",
                        value = 0.95,
                        min = 0.0,
                        max = 1.0
                        ,step = 0.01
                      ),
                      numericInput(
                        inputId = "parameter4",
                        label = "positive/negative interval (% of PRI/2)",
                        value = 1.0,
                        min = 0.0,
                        max = 1.0
                        ,step = 0.01
                      )
                    ),
                    conditionalPanel(
                      condition = "input.audiogeneratorsignal=='custom'",
                      selectizeInput(
                        # textInput(
                        inputId = "customsignalinput",
                        label = "Custom signal-input:",
                        # value = "",
                        # placeholder = "Enter R-commands here"
                        choices = c(
                              `white-noise`='nSecsDuration=3;Fs=8000;xnWave=tuneR::noise(kind="white",duration=nSecsDuration*Fs,samp.rate=Fs,xunit="samples");xn=xnWave@left;list(xn=xn,Fs=Fs)',
                              `pink-noise`='nSecsDuration=3;Fs=8000;xnWave=tuneR::noise(kind="pink",duration=nSecsDuration*Fs,samp.rate=Fs,xunit="samples");xn=xnWave@left;list(xn=xn,Fs=Fs)',
                              `pulsed`='nSecsDuration=3;Fs=8000;frq=1;xnWave=tuneR::pulse(freq=frq,from=0,duration=nSecsDuration*Fs,samp.rate=Fs,width=0.1,plateau=0.2,interval=0.5,xunit="samples");xn=xnWave@left;list(xn=xn,Fs=Fs)',
                              `saw-tooth`='nSecsDuration=3;Fs=8000;frq=1;xnWave=tuneR::sawtooth(freq=frq,from=0,duration=nSecsDuration*Fs,samp.rate=Fs,reverse=FALSE,xunit="samples");xn=xnWave@left;list(xn=xn,Fs=Fs)',
                              `silence`='nSecsDuration=3;Fs=8000;xnWave=tuneR::silence(from=0,duration=nSecsDuration*Fs,samp.rate=Fs,xunit="samples");xn=xnWave@left;list(xn=xn,Fs=Fs)',
                              `sine-wave`='nSecsDuration=3;Fs=8000;frq=1;xnWave=tuneR::sine(freq=frq,from=0,duration=nSecsDuration*Fs,samp.rate=Fs,xunit="samples");xn=xnWave@left;list(xn=xn,Fs=Fs)',
                              `square-wave`='nSecsDuration=3;Fs=8000;frq=1;xnWave=tuneR::square(freq=frq,from=0,duration=nSecsDuration*Fs,samp.rate=Fs,up=0.5,xunit="samples");xn=xnWave@left;list(xn=xn,Fs=Fs)',
                              `heavi-sine`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.HEAVI.SINE,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `bumps`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.BUMPS,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `blocks`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.BLOCKS,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `doppler`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.DOPPLER,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `ramp`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.RAMP,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `cusp`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.CUSP,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `sing`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.SING,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `hi-sine`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.HI.SINE,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `lo-sine`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.LO.SINE,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `lin-chirp`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.LIN.CHIRP,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `two-chirp`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.TWO.CHIRP,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `quadratic-chirp`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.QUAD.CHIRP,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `mish-mash`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.MISH.MASH,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `wernersorrows`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.WERNER.SORROWS,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `leopold`='nSecsDuration=3;Fs=8000;xn=rwt::makesig(SIGNAL.LEOPOLD,N=nSecsDuration*Fs)$x;list(xn=xn,Fs=Fs)',
                              `astsa-soi`='xn=astsa::soi;Fs=(length(xn)/0.3775);list(xn=xn,Fs=Fs)',
                              `astsa-cmort`='xn=astsa::cmort;Fs=(length(xn)/0.508);list(xn=xn,Fs=Fs)',
                              `astsa-star`='xn=astsa::star;Fs=(length(xn)/0.197);list(xn=xn,Fs=Fs)',
                              `astsa-nyse`='xn=astsa::nyse;Fs=(length(xn)/0.794);list(xn=xn,Fs=Fs)',
                              `sunspot-monthly`='xn=datasets::sunspot.month;Fs=(length(xn)/0.265);list(xn=xn,Fs=Fs)',
                              `arima(2,0,2) noise`='N=1024;Fs=4000;b=c(-0.2279,0.2488);a=c(0.8897,-0.4858);Tseasonal=12;xn=arima.sim(n=N,list(order=c(2,0,2),ma=b,ar=a,seasonal=list(order=c(0,0,0),period=Tseasonal)),rand.gen=rnorm,sd=sqrt(0.1796));list(xn=xn,Fs=Fs)',
                              `shifted sinc-function`='nSamples=1000000;nSecsDuration=40;Fs=(nSamples-1)/(nSecsDuration);tn=seq(0,nSecsDuration,by=1/Fs);xn=rep(1,times=nSamples);for (i in 1:nSamples) {if (abs(tn[i]-nSecsDuration/2)<(2*eps)) {xn[i]=1} else {xn[i]=sin(pi*(tn[i]-nSecsDuration/2))/(pi*(tn[i]-nSecsDuration/2))}};list(xn=xn,Fs=Fs)',
                              `a sum of five-tonals (sine-waves)`='nSecsDuration=3;Fs=8000;tn=seq(0,nSecsDuration,by=1/Fs);xn=5+1.5*sin(0.2*Fs*pi*tn)+1.3*cos(0.4*Fs*pi*tn)-0.9*sin(0.5*Fs*pi*tn)-0.5*cos(0.6*Fs*pi*tn);list(xn=xn,Fs=Fs)',
                              `Doppler (audible-range)`='nSecsDuration=4;Fs=8000;tn=seq(0,nSecsDuration,by=1/Fs);f0Hz=1000;xn=sqrt(tn*(nSecsDuration-tn))*sin((2*pi*f0Hz*1.05)/(tn+0.05));list(xn=xn,Fs=Fs)'
                              # ,`( random-signal from this list )` = paste0(
                              #   "sample(c('",
                              #   paste(
                              #     'nSecsDuration=3;Fs=8000;xnWave=tuneR::noise(kind="red",duration=nSecsDuration*Fs,samp.rate=Fs);xn=xnWave@left;list(xn=xn,Fs=Fs)',
                              #     'nSamples=1024;Fs=(nSamples-1)/20;tn=seq(-10,10,by=1/Fs);for (i in 1:nSamples) {if (abs(tn[i])<(2*eps)) {xn[i]=1} else {xn[i]=sin(pi*tn[i])/(pi*tn[i])}};list(xn=xn,Fs=Fs)',
                              #     'nSecsDuration=3;Fs=8000;tn=seq(0,nSecsDuration,by=1/Fs);xn=5+1.5*sin(0.2*pi*tn)+1.3*cos(0.4*pi*tn)-0.9*sin(0.5*pi*tn)-0.5*cos(0.6*pi*tn);list(xn=xn,Fs=Fs)',
                              #     'nSecsDuration=4;Fs=8000;tn=seq(0,nSecsDuration,by=1/Fs);f0Hz=300;xn=sqrt(tn*(1-tn))*sin((2*pi*f0Hz*1.05)/(tn+0.05));list(xn=xn,Fs=Fs)',
                              #     sep = "','"
                              #   ),
                              #   "'), 1 )"
                              # )
                              # )
                            ),
                            # selected = sample(
                            #   c(
                            #     'nSecsDuration=3;Fs=8000;xnWave=tuneR::noise(kind="red",duration=nSecsDuration*Fs,samp.rate=Fs);xn=xnWave@left;list(xn=xn,Fs=Fs)',
                            #     'nSamples=1024;Fs=(nSamples-1)/20;tn=seq(-10,10,by=1/Fs);xn=rep(1,times=nSamples);for (i in 1:nSamples) {if (abs(tn[i])<(2*eps)) {xn[i]=1} else {xn[i]=sin(pi*tn[i])/(pi*tn[i])}};list(xn=xn,Fs=Fs)',
                            #     'nSecsDuration=3;Fs=8000;tn=seq(0,nSecsDuration,by=1/Fs);xn=5+1.5*sin(0.2*pi*tn)+1.3*cos(0.4*pi*tn)-0.9*sin(0.5*pi*tn)-0.5*cos(0.6*pi*tn);list(xn=xn,Fs=Fs)',
                            #     'nSecsDuration=4;Fs=8000;tn=seq(0,nSecsDuration,by=1/Fs);f0Hz=300;xn=sqrt(tn*(1-tn))*sin((2*pi*f0Hz*1.05)/(tn+0.05));list(xn=xn,Fs=Fs)'
                            #     ),
                            #   1
                            # ),
                            multiple = FALSE,
                            options = list(create = TRUE,
                                           placeholder = "type here, or pull-down for examples"
                                           )
                        ),
                        textInput(
                            inputId = "edit_customsignalText",
                            label = NULL,
                            value = ""
                            ,width = '100%' # '400px'
                            ,placeholder = 'enter R-commands here'
                          )
                      ,shinyBS::bsButton(
                                  inputId = "pb_generatecustom",
                                  label = "Evaluate custom",
                                  # width = "31.5%",
                                  style = "default", # default, primary, success, info, warning, or danger
                                  size = "default", # extra-small, small, default, or large
                                  type = "action", # action, toggle
                                  block = FALSE,
                                  disabled = FALSE
                                  # ,icon = shiny::icon("file-audio-o", lib = "font-awesome") # "play" # "music" #
                                )
                      # ,numericInput(inputId = "Fscustom",
                      #              label = "Custom sampling-freq, Fs [Hz=Samples-per-sec])",
                      #              value = 8000,
                      #              min = 0,
                      #              max = 192000
                      #              ,step = 100)
                    )

                    # ,shinyBS::bsButton(
                    #   inputId = "pb_generateaudio",
                    #   label = "Generate the Signal",
                    #   # width = "31.5%",
                    #   style = "default", # default, primary, success, info, warning, or danger
                    #   size = "default", # extra-small, small, default, or large
                    #   type = "action", # action, toggle
                    #   block = FALSE,
                    #   disabled = FALSE,
                    #   icon = shiny::icon("barcode", lib = "font-awesome") # http://fontawesome.io/icons/
                    # )
                  ),
                  br(),
                  verbatimTextOutput(outputId = "filenameAudioInfo"),
                  hr(),
                  shinyBS::bsButton(
                                  inputId = "pb_playfile",
                                  label = HTML("Play (un-filtered) file, x<sub>[n]</sub>"),
                                  # width = "31.5%",
                                  style = "default", # default, primary, success, info, warning, or danger
                                  size = "default", # extra-small, small, default, or large
                                  type = "action", # action, toggle
                                  block = FALSE,
                                  disabled = FALSE,
                                  icon = shiny::icon("play", lib = "font-awesome") # "file-audio-o"  # "play" # "music" #
                                ),
                  shinyBS::bsButton(
                                  inputId = "pb_playFiltered",
                                  label = HTML("Play Filtered-file, y<sub>[n]</sub>"),
                                  # width = "31.5%",
                                  style = "default", # default, primary, success, info, warning, or danger
                                  size = "default", # extra-small, small, default, or large
                                  type = "action", # action, toggle
                                  block = FALSE,
                                  disabled = FALSE,
                                  icon = shiny::icon("filter", lib = "font-awesome") # "file-audio-o"  # "play" # "music" #
                                ),
                  br(),
                  br(),
                  uiOutput(outputId = "HTML5audioWidget"),
                  br(),
                  uiOutput(outputId = "HTML5audioWidget2"),
                  br(),
                  checkboxInput(
                    inputId = "includeContoursInSpectrograms",
                    label = "Include Contours in Spectrograms",
                    value = FALSE
                    ),
                  uiOutput(outputId = "stretchyslider3rangeWidget")
                  ), # end column
                  column(width=9,
                       if (scalePlotsToVerticalHeight) {
                            tags$head(tags$style(
                              paste0(
                                "#specgrams{height:",
                                verticalHeightOfPlots,
                                " !important;}"
                              )
                            ))
                          },
                         plotOutput(
                           outputId = "specgrams",
                           width = "100%",
                           height = "650px",
                           inline = FALSE
                         ),
                       shinyBS::bsCollapse(
                          id = "audioPanelCollapse",
                          shinyBS::bsCollapsePanel(
                            value = "audioTimePanelCollapse1",
                            title = "Audio Time-Domain Panel (click to expand/ collapse):",
                            style = "info",
                            if (scalePlotsToVerticalHeight) {
                              tags$head(tags$style(
                                paste0(
                                  "#signaltimeplots{height:",
                                  verticalHeightOfPlots,
                                  " !important;}"
                                )
                              ))
                              },
                         plotOutput(
                           outputId = "signaltimeplots",
                           width = "100%",
                           height = "650px",
                           inline = FALSE
                         )
                          ))
                ) # end column
              ) # end tags$span
            ) # end fluidRow
          ) # shinyBS::bsCollapsePanel
          ) # shinyBS::bsCollapse
          ,data.step=4, 
          data.intro="This is the Audio-Applications panel.", 
          data.hint="Hint: This is the Audio-Applications panel."
          ) # end rintrojs::introBox
        ), # end tabPanel
        # ,data.step=1, 
        # data.intro="This is the main plots panel.", 
        # data.hint="Hint: This is the main plots panel."
        # ) # end rintrojs::introBox
        # 'More' navbarMenu ----
        navbarMenu(
          title = "More",
          icon = shiny::icon("navicon", lib = "font-awesome"),
          tabPanel(
            style = paste0("background-color: ", NavBarPageBackgroundColor,
                           ";"),
            title = "Background/ Theory",
            icon = shiny::icon("list-alt",
                               lib = "font-awesome"),
            helpText("Welcome to the Background/ Theory Page..."),
            br(),
            wellPanel(
              style = paste("background-color:",
                            WellPanelBackgroundColor, ";"),
              HTML(markdown::markdownToHTML(fragment.only=TRUE, text="
# References:

* J.J. Allaire, J. Cheng, Y. Xie, J. McPherson, W. Chang, J. Allen, H. Wickham, A. Atkins, and R. Hyndman. `rmarkdown`: Dynamic Documents for R, 2016. R package version 1.0.
* J.J. Allaire, J. Horner, V. Marti, and N. Porte. `markdown`: 'Markdown' Rendering for R, 2015. R package version 0.7.7.
* C. Beeley. Web Application Development with R Using `shiny`` -- Second Edition. Packt Publishing, Limited, Birmingham, 2nd;2;2; edition, 2016.
* H. Bengtsson. `R.matlab`: Read and Write .mat-type Files and Call MATLAB from within R, 2016. R package version 3.6.0.
* H.W. Borchers. `pracma`: Practical Numerical Math Functions, 2016. R package version 1.9.3.
* J.R. Buck, K.E. Wage, and M.A. Hjalmarson. Item response analysis of the continuous-time signals and systems concept inventory. In 2009 IEEE 13th Digital-Signal Processing Workshop and 5th IEEE Signal Processing Education Workshop, pages 726--730, Jan 2009.
* W. Chang, J. Cheng, J.J. Allaire, Y. Xie, and J. McPherson. `shiny`: Web Application Framework for R, 2016. R package version 0.13.2.
* J. Dobes. An accuracy comparison of the digital filter poles-zeros analysis. Radioengineering, 12(4):0--, Dec. 2003.
* N.D. Fleming and C. Mills. Not another inventory, rather a catalyst for reflection. 1992.
* A.M. Goncher, D. Jayalath, and W. Boles. Insights into students' conceptual understanding using textual analysis: A case study in signal processing. IEEE Transactions on Education, 59(3):216--223, Aug 2016.
* J. Gruber. `Markdown`` -- A text-to-HTML conversion-tool for the web, Dec 2004. website: http://daringfireball.net/projects/markdown/.
* F.J. Harris. On the use of windows for harmonic analysis with the discrete Fourier transform. Proceedings of the IEEE, 66(1):51--83, 1978.
* D.E. Hiebeler. R and MATLAB. CRC Press [Imprint], Abingdon, 1st edition, 2015.
* J. Hillebrand and M.H. Nierhoff. Mastering `RStudio`: Develop, Communicate, and Collaborate with R. Packt Publishing, 1 edition, 2015.
* M. Hopkins. Relating continuous time and discrete time in the classroom. In 2008 Annual Conference and Exposition, Pittsburgh, Pennsylvania, June 2008. ASEE Conferences.
* B.D. Ictenbas and H. Eryilmaz. Determining learning styles of engineering students to improve the design of a service course. Procedia -- Social and Behavioral Sciences, 28:342--346, 2011.
* D.H. Johnson, P. Prandoni, P.C. Pinto, and M. Vetterli. Teaching signal processing online: A report from the trenches. pages 8786--8790. IEEE, 2013.
* D.E. Knuth. Literate programming. The Computer Journal, 27(2):97--111, 1984.
* U. Ligges, S. Krey, O. Mersmann, and S. Schnackenberg. `tuneR`: Analysis of music, 2014.
* J. MacFarlane. `pandoc`: A universal document converter, October 2016. website: http://pandoc.org.
* J.H. McClellan, R. Schafer, and M.A. Yoder. Signal Processing First. Always learning. Prentice Hall Higher Education, 2015.
* J.H. McClellan, R.W. Schafer, and M.A. Yoder. Experiences in teaching DSP First in the ECE curriculum. In 1997 IEEE International Conference on Acoustics, Speech, and Signal Processing, ICASSP '97, Munich, Germany, April 21-24, 1997 [1], pages 19--22.
* J.H. McClellen and M.A. Yoder. DSP First: A Multimedia Approach. Prentice Hall PTR, Upper Saddle River, NJ, USA, 1st edition, 1997.
* O. Mersmann and U. Ligges and S. Krey and more. `signal`: Signal processing, 2014.
* T. Ogunfunmi. Analysis of assessment using signals and systems concept inventory for systems courses. In 2011 IEEE International Symposium of Circuits and Systems (ISCAS), pages 595--598, May 2011.
* T. Ogunfunmi, G.L. Herman, and M. Rahman. On the use of concept inventories for circuits and systems courses. IEEE Circuits and Systems Magazine, 14(3):12--26, thirdquarter 2014.
* A.V. Oppenheim, R.W. Schafer, and J.R. Buck. Discrete-time signal processing. Prentice Hall International, Upper Saddle River, N.J, 2nd edition, 1999.
* M. Otto, J. Thornton, and more. `Bootstrap`: An HTML, CSS, and JS framework for developing responsive, mobile-first projects on the web, July 2016. website: http://getbootstrap.com.
* R Core Team. R: A Language and Environment for Statistical Computing. R Foundation for Statistical Computing, Vienna, Austria, 2016.
* J.O. Ramsay, G. Hooker, and S. Graves. Functional data analysis with R and MATLAB. Springer, Dordrecht;New York;, 1. aufl. edition, 2009.
* H.G. Resnizky. Learning `shiny`: make the most of R's dynamic capabilities and create web applications with shiny. Packt Publishing, Birmingham, 2015.
* RStudio. Authoring pandoc markdown, Apr 2016. website: http://rmarkdown.rstudio.com/authoring_pandoc_markdown.html.
* RStudio Team. `RStudio`: Integrated Development Environment for R. RStudio, Inc., Boston, MA, 2015.
* R.H. Shumway and D.S. Stoffer. Time series analysis and its applications: with R examples. Springer, New York, 3rd edition, 2011.
* J.O. Smith. Introduction to Digital Filters with Audio Applications. 2007. online book: http://ccrma.stanford.edu/~jos/filters/.
* J.O. Smith. Introduction to Digital Filters: with Audio Applications, volume 2. Julius Smith, 2008. website: https://www.dsprelated.com/freebooks/filters/.
* J.O. Smith. Spectral Audio Signal Processing. Online book, 2011 edition, website: https://ccrma.stanford.edu/~jos/sasp/sasp.html.
* S.W. Smith. The scientist and engineer's guide to digital signal processing. 1997. website: http://www.DSPguide.com.
* B.L. Sturm and J.D. Gibson. Signals and systems using MATLAB (SSUM): an integrated suite of applications for exploring and teaching media signal processing. pages F2E–21. IEEE, 2005.
* S. Urbanek. `audio`: Audio Interface for R, 2013. R package version 0.1-5.
* S. Urbanek. `png`: Read and write PNG images, 2013. R package version 0.1-7.
* A.A. Ursani, A.A. Memon, and B.S. Chowdhry. Bloom's taxonomy as a pedagogical model for signals and systems. International Journal of Electrical Engineering Education, 51(2):162--173, 2014.
* K.E. Wage, J.R. Buck, C.H.G. Wright, and T.B. Welch. The signals and systems concept inventory. IEEE Transactions on Education, 48(3):448--461, Aug 2005.
* Y. Xie. `knitr`: A comprehensive tool for reproducible research in R. In V. Stodden, F. Leisch, and R.D. Peng, editors, Implementing Reproducible Computational Research. Chapman and Hall/CRC, 2014. ISBN 978-1466561595.
* Y. Xie. Dynamic Documents with R and `knitr`. Chapman and Hall/CRC, Boca Raton, Florida, 2nd edition, 2015. ISBN 978-1498716963.
* Y. Xie. `knitr`: A General-Purpose Package for Dynamic Report Generation in R, 2016. R package version 1.14.
* Udo Zolzer. Digital audio signal processing. Wiley, Chichester, U.K, 2nd edition, 2008.
* Udo Zolzer and Xavier Amatriain. DAFX: digital audio effects. Wiley, Hoboken, NJ;Chichester, West Sussex;, 2002.
"
              )) # end markdownToHTML / HTML
            ) # end wellPanel
          ), # end tabPanel
          # . Settings Page ----
          tabPanel(
            style = paste("background-color:", NavBarPageBackgroundColor, ";"),
            title = "My Settings",
            icon = shiny::icon("table", lib = "font-awesome"),
            wellPanel(
              style = paste("background-color:",
                            WellPanelBackgroundColor, ";"),
              helpText("Welcome to the MySettings Page ..."),
              br(),
              shinyBS::bsButton(
                            inputId = "pb_runintro",
                            label = "Start guided-tour",
                            # width = "31.5%",
                            style = "primary",
                            # size = "small",
                            # type = "toggle",
                            block = FALSE,
                            disabled = FALSE,
                            value = FALSE,
                            icon = shiny::icon("map-o", lib = "font-awesome")
              ),
              br(),
              hr(),
              colourpicker::colourInput(
                inputId = "grcolor",
                label = "Grid-Colour for plots (or just choose 'transparent' for none)",
                palette = "limited",
                allowedCols = grey.colors(40L, start = 0L, end = 1L),
                value = "#BCBCBC",
                allowTransparent = TRUE,
                returnName = TRUE
              ),
              checkboxInput(
                inputId = "degreesgrid",
                label = "Use degrees for polar grid-labels (otherwise, radians)",
                value = FALSE
              ),
              checkboxInput(
                inputId = "polargrid",
                label = "Add polar-grid (\\(r,\\theta\\))",
                value = TRUE
              ),
              checkboxInput(
                inputId = "rootlocusgrid",
                label = "Add root-locus grid: damping-ratio (\\(\\zeta\\)), and natural-frequency (\\(\\omega_n\\)), from s-plane to z-plane",
                value = FALSE
              ),
              checkboxInput(
                inputId = "showUnitCircle",
                label = "Show unit-circle on pole-zero/3D plots",
                value = TRUE
              ),
              checkboxInput(
                inputId = "show3DSurface",
                label = "Show surface on 3D plots",
                value = TRUE
              ),
              checkboxInput(
                inputId = "show3DColoured",
                label = "Show coloured-surface on 3D plots (else, just shiny-grey)",
                value = TRUE
              ),
              checkboxInput(
                inputId = "checkboxRAY",
                label = "Use ray-cursor on plots",
                value = TRUE
              ),
              checkboxInput(
                inputId = "showLegend",
                label = "Use legends on (multi-line) plots",
                value = FALSE
              ),
              checkboxInput(
                inputId = "secondaryaxis",
                label = "Include secondary axis",
                value = TRUE
              ),
              checkboxInput(
                inputId = "scaletoverticalheight",
                label = "Scale all plots to the browser's vertical-height",
                value = TRUE
              ),
              br(),
              numericInput(
                inputId = "LineWidth",
                label = "Line-Width within plots",
                value = 3L,
                min = 0.5,
                max = 6L,
                step = 0.5
              ),
              br(),
              colourpicker::colourInput(
                inputId = "ForegroundColor",
                label = "Line-Colour for plotted-lines/ points",
                palette = "limited",
                value = "blue",
                allowTransparent = TRUE,
                returnName = TRUE
              ),
              br(),
              colourpicker::colourInput(
                inputId = "ForegroundColorPoles",
                label = "Line-Colour for pole-symbols",
                palette = "limited",
                value = "red",
                allowTransparent = TRUE,
                returnName = TRUE
              ),
              colourpicker::colourInput(
                inputId = "ForegroundColorZeros",
                label = "Line-Colour for zero-symbols",
                palette = "limited",
                value = "green",
                allowTransparent = TRUE,
                returnName = TRUE
              ),
              br(),
              colourpicker::colourInput(
                inputId = "BackgroundColor",
                label = "Background-Fill Colour for open-type (e.g. circle, square, etc) plot-symbols",
                palette = "limited",
                value = "transparent",
                allowTransparent = TRUE,
                returnName = TRUE
              ),
              br(),
              checkboxInput(
                inputId = "twosidedFFT",
                label = "Display Two-Sided FFTs",
                value = TRUE
              ),
              checkboxInput(
                inputId = "FFTshifted",
                label = "FFT shifted downwards to straddle zero",
                value = TRUE
              ),
              checkboxInput(
                inputId = "logarithmicFreqAxis",
                label = "Logarithmic-scale for Frequency-Axis",
                value = FALSE
              ),
              numericInput(
                inputId = "minimumLogFreqDisplayed",
                label = "minimum (normalized) Log freq to be displayed",
                value = 0.001,
                # min = 0.000001,
                max = 1.0
                # ,step = 0.0001
              ),
              br(),
              checkboxInput(
                inputId = "showPhaseOnMagPlot",
                label = "Show Phase on Magnitude-plots",
                value = FALSE
              ),
              checkboxInput(
                inputId = "showMaxMinsOnMagPlot",
                label = "Show Maximums (i.e. Side-Lobe Levels)/ Minimums on Magnitude-plots",
                value = TRUE
              ),
              checkboxInput(
                inputId = "unwrapPhase",
                label = HTML(
                  "Unwrap Phase (i.e. add multiples of ",
                  "\\(2\\pi\\)",
                  ", as appropriate, in order to remove 'jumps')"
                ),
                value = FALSE
              ),
              checkboxInput(
                inputId = "normalizedMagPlotAmplitude",
                label = "normalize the Magnitude-Response's plotted-amplitude to unity, 1 (or to 0 dB)",
                value = TRUE
              ),
              checkboxInput(
                inputId = "logarithmicMagPlotAmplitude",
                label = "Logarithmic-scale for plotting Magnitude-Response's relative-amplitude (i.e. wrt 0 dB at max)",
                value = TRUE
              ),
              br(),
              radioButtons(
                inputId = "freqaxisunits",
                label = "Frequency-Axis Units",
                choices = c(
                  `0L --> 1` = "zero2one",
                  `0L --> 3.1415... (\\(=\\pi\\))` = "zero2pi",
                  `0L --> 6.2832... (\\(=2\\pi\\)), one-sided` = "zero22pi",
                  `0L --> 0.5` = "zero2half",
                  `0L --> fs/2 (specify)` = "zero2fsby2",
                  `0L --> \\(f_{max}\\), one-sided (specify)` = "zero2fmax"
                ),
                selected = "zero2one",
                inline = TRUE
              ),
              conditionalPanel(
                condition = "input.freqaxisunits == 'zero2fs' || input.freqaxisunits == 'zero2fsby2'",
                numericInput(
                  inputId = "samplingfreq",
                  label = "Sampling-Frequency \\(f_s\\)",
                  value = 44100,
                  min = 1000,
                  max = 3e+12,
                  step = 1000
                )
              ),
              conditionalPanel(
                condition = "input.freqaxisunits == 'zero2fmax' || input.freqaxisunits == 'zero2fmaxby2'",
                numericInput(
                  inputId = "maxfreqsampled",
                  label = "Max-Frequency being sampled \\(f_{max}\\)",
                  value = 44100 /
                    2L,
                  min = 1000,
                  max = 3e+12,
                  step = 100L
                )
              ),
              br(),
              colourpicker::colourInput(
                inputId = "BrushFillColor",
                label = "Brush-Fill Colour (for inside of selection-rectangles)",
                palette = "square",
                value = "#9CF",
                allowTransparent = TRUE,
                returnName = TRUE
              ),
              colourpicker::colourInput(
                inputId = "BrushOutlineColor",
                label = "Brush-Outline ('rubberband') Colour (for edge of selection-rectangles)",
                palette = "square",
                value = "#036",
                allowTransparent = TRUE,
                returnName = TRUE
              ),
              br(),
              hr(),
              br(),
              actionButton("resetAll",
                           "Reset all input-elements back to original-values"),
              br(),
              hr(),
              br(),
              bookmarkButton(
                label = "Bookmark...",
                icon = shiny::icon(name = "link", lib = "glyphicon"),
                title = "Bookmark this application's state, and get a URL for sharing."
              )
            )
          )
        ), # end navbarMenu
        # 'About' tabPanel ----
        tabPanel(
          style = paste("background-color:", NavBarPageBackgroundColor, ";"),
          title = "About",
          icon = shiny::icon("info", lib = "font-awesome"),
          helpText("Welcome to the ", tags$em("About"), " Page..."),
          br(),
          wellPanel(
            style = paste("background-color:",
                            WellPanelBackgroundColor, ";"),
          tags$span(
            title = "tooltip: About Page/ Tab",
            p("The source-code listing is available on ",
              a("GitHub", href="https://github.com/popeye00/inSPiRe"),
              ".  Collaborations are welcomed."),
            br(),
            p("The web-app is temporarily hosted at ",
              a("inspire.shinyapps.io", href = "https://inspire.shinyapps.io/PeZdemoR/"),
              ", with a limited ",
              a("25 hours per month as a free-account",href = "http://www.shinyapps.io/#pricing"),
              ".  Other paid/ free hosting solutions also exist: either online at shinyapps.io, self-served by using ",
              a("Linux server", href="https://www.rstudio.com/products/shiny/shiny-server/"),
              "or (preferably) locally-served by your own running RStudio application."
              ),
            br(),
            h1("Introducing Shiny"),
            p(
              "Shiny is a new package from RStudio that makes it ",
              em("incredibly easy"),
              " to build interactive web-applications with R."
            ),
            a(img(
              src = "bigorb.png",
              height = 72L,
              width = 72L
            ), href = "http://www.rstudio.com/shiny"),
            br(),
            p(
              "For an introduction and live examples, visit the ",
              a("Shiny homepage", href = "http://www.rstudio.com/shiny")
            ),
            tags$link("http://www.rstudio.com/shiny"),
            br(),
            h2("Features"),
            tags$ul(
              tags$li(
                "Build useful web-applications with only a few lines of ",
                code("code"),
                " -- no ",
                code("JavaScript"),
                " required."
              ),
              tags$li(
                "Shiny applications are automatically 'live', in the same way that ",
                strong("spreadsheets"),
                " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser."
              )
            ),
            hr(),
            # tags$div(HTML(
            #   paste(
            #     tags$span(style = "color:red",
            #               "This text"),
            #     " is ",
            #     tags$span(style = "color:red",
            #               "red"),
            #     sep = ""
            #   )
            # )),
            HTML(markdown::markdownToHTML(fragment.only=TRUE,text="This web-app: &copy; 2016-2017 P. Squires, DiYZeR Research Group, ECE Dept, University of Victoria"))
            ,uiOutput(outputId = "AboutPageWidget")
          ) # end tooltip span
          ) # end wellPanel
        ), # end tabPanel
        # # http://stackoverflow.com/questions/28967949/add-a-twitter-share-button-to-shiny-r-navbar
        # tags$script(HTML("var header = $('.navbar > .container');
        #                header.append('<div style=\"float: right\"><a href=\"https://twitter.com/share\" class=\"twitter-share-button\" aling=\"middle\" data-url=\"www.mywebsite.com\" data-text=\"Visit www.mywebsite.com\" data-size=\"large\">Tweet</a></div>');
        #                console.log(header)")),
        # tags$script(HTML("!function(d,s,id){
        #     var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
        #     if(!d.getElementById(id)){
        #             js=d.createElement(s);
        #             js.id=id;
        #             js.src=p+'://platform.twitter.com/widgets.js';
        #             fjs.parentNode.insertBefore(js,fjs);
        #     }
        #   }(document, 'script', 'twitter-wjs');"
        #                  )
        #             ),
        
        # (navbar) Footer - common below all tabPanels ----
        footer = div(
          class = "footer",
          style = "font-size:8pt;",
          hr(),
          tags$span(title = "tooltip: pixelratio",
                    wellPanel(
                      class = "well-sm",
                      style = paste0("background-color: ",
                                     WellPanelBackgroundColor, ";"),
                      uiOutput("pixelratio")
                      )
          ),
          # https://www.addtoany.com/buttons/for/website
          # tag$div(
            HTML('<!-- AddToAny BEGIN -->
<div class="a2a_kit a2a_kit_size_32 a2a_default_style">
<a class="a2a_dd" href="https://www.addtoany.com/share?linkurl=https%3A%2F%2Finspire.shinyapps.io%2FPeZdemoR%2F&amp;linkname=PeZdemoR"></a>
<a class="a2a_button_facebook"></a>
<a class="a2a_button_twitter"></a>
<a class="a2a_button_google_plus"></a>
<a class="a2a_button_linkedin"></a>
<a class="a2a_button_email"></a>
</div>
<script>
var a2a_config = a2a_config || {};
a2a_config.linkname = "PeZdemoR";
a2a_config.linkurl = "https://inspire.shinyapps.io/PeZdemoR/";
a2a_config.prioritize = ["facebook", "twitter", "google_plus", "linkedin", "email"];
</script>
<script async src="https://static.addtoany.com/menu/page.js"></script>
<!-- AddToAny END -->'
            ),
          hr()
          # # https://github.com/daattali/shiny-server/blob/master/simple-index.html
          # ,HTML('<script type="text/javascript" src="//s7.addthis.com/js/300/addthis_widget.js#pubid=ra-57d65a4d97789dd1"></script>')
          # )
        ) # end footer
      ) # end navbarPage
    )
  )

# SERVER ************************************************************* ----
server <- shinyServer(function(input, output, session) {
  shinyjs::hide(selector = "#ResultsPanel li a[data-value=Tbl]")
  shinyjs::hide(selector = "#ResultsPanel li a[data-value=All]")
  shinyjs::hide(selector = "#pzPlotsPanel li a[data-value=3js]")
  
  # # initiate hints on startup with custom button and event
  # rintrojs::hintjs(session, 
  #                  options = list("hintButtonLabel"="Hope this hint was helpful"),
  #                  events = list("onhintclose"='alert("Wasn\'t that hint helpful")')
  # )
  
  handles <-
    reactiveValues(
      poleloc = c(0L),
      zeroloc = c(0L),
      selectedpole = NULL,
      selectedzero = NULL,
      maxZoomlimXpassband = 1L * isolate(input$samplingfreq) / 2,
      maxZoomlimYpassband = 1L,
      maxZoomlimXstopband = 1L * isolate(input$samplingfreq) / 2,
      maxZoomlimYstopband = 1L,
      minZoomlimXpassband = 0L * isolate(input$samplingfreq) / 2,
      minZoomlimYpassband = -1L,
      minZoomlimXstopband = 0L * isolate(input$samplingfreq) / 2,
      minZoomlimYstopband = -1L,
      stepZoomlimXpassband = 0.05,
      stepZoomlimYpassband = 0.25,
      stepZoomlimXstopband = 0.05,
      stepZoomlimYstopband = 0.25,
      slider1value = 0.5 *
        isolate(input$samplingfreq) / 2,
      growSliders = 0.2,
      inDragMode = FALSE,
      changed = FALSE,
      subscriptOfClickedPoleOrZero = NULL,
      minslider1range = 0L * isolate(input$samplingfreq) / 2,
      maxslider1range = 5L * isolate(input$samplingfreq) / 2,
      minslider1step = 0.001,
      maxslider1step = 0.5,
      minslider1animinterval = 0L,
      maxslider1animinterval = 2000L,
      minslider3range = 0L,
      maxslider3range = 1L,
      inputstretchyslider3step = 0.01,
      minslider3step = 0.01,
      maxslider3step = 1.0,
      minslider3animinterval = 0L,
      maxslider3animinterval = 2000L,
      slider3value = 0.5,
      minslider2Arange = 0L,
      maxslider2Arange = 5L,
      minslider2Astep = 0.001,
      maxslider2Astep = 1.0,
      minslider2Aaniminterval = 0L,
      maxslider2Aaniminterval = 2000L,
      slider2Avalue = 0.5,
      minslider2Brange = 0L,
      maxslider2Brange = 5L,
      minslider2Bstep = 0.001,
      maxslider2Bstep = 1.0,
      minslider2Baniminterval = 0L,
      maxslider2Baniminterval = 2000L,
      slider2Bvalue = 0.5
    )
  
  # observeEvent resetAll ----
  observeEvent(eventExpr = input$resetAll, handlerExpr = {
    shinyjs::reset("myMainFluidPage")
    shinyjs::info("Done.  All input-elements\n are now reset.")
  })
  
  output$AboutPageWidget <- renderUI({
    absolutePanel(
      top = (input$dimension[2]-400)/2, # "25%", # 20, # see pixelratio: input$dimension[1] (width) by input$dimension[2] (height)
      left = (input$dimension[1]-400)/2, # "25%", # 20,
      width = 400,
      draggable = TRUE, # <<< Note!
      wellPanel(
        style = paste0("background-color: ", AppBackgroundColor, ";",
                       "border:4px; border-color:#458cc3;"
                       ),
        helpText("Welcome to the ", tags$em("About"), " Page..."),
        br(),
        HTML(
          markdown::markdownToHTML(
            fragment.only = TRUE,
            text = paste0(
"This is a sample web-application made with Shiny to demonstrate some basic DSP concepts.

This panel is an `absolutePanel` that uses `top` and `left` attributes.

It also has `draggable = TRUE`, so you can drag it to move it around the page.

The slight transparency is due to `style = 'opacity: 0.9'`.

You can put anything into `absolutePanel`, including any Shiny inputs and outputs."
            ) # end paste0
          ) # end markdownToHTML
        ), # end HTML
        a(img(
              src = "favicon.png",
              height = 36L,
              width = 36L
            ), href = "http://www.rstudio.com/shiny"),
        HTML(markdown::markdownToHTML(fragment.only=TRUE,text="This web-app: &copy; 2016-2017 P. Squires, DiYZeR Research Group, ECE Dept, University of Victoria"))
      ), # end wellPanel
      style = "opacity: 0.9" # semi-transparent
    ) # end absolutePanel
  })
  
  # # observeEvent pb_music ----
  # observeEvent(eventExpr = input$pb_music, handlerExpr = {
  #   handel <- R.matlab::readMat("handel.mat") # y= audio-signal, and Fs=8192 samples-per-second
  #   y <- handel$y
  #   Fs <- handel$Fs
  #   updateNumericInput(session, inputId = "samplingfreq", value = Fs)
  #   y <- y/max(abs(y)) * 1.0 #  tuneR::normalize(as.vector(y),unit=1) #    
  #   require(audio); try(audio::wait(audio::play(y, rate=Fs)),silent=TRUE)
  # })
  # 
  # # observeEvent pb_musicfiltered ----
  # observeEvent(eventExpr = input$pb_musicfiltered, handlerExpr = {
  #   handel <- R.matlab::readMat("handel.mat") # y= audio-signal, and Fs=8192 samples-per-second
  #   y <- handel$y
  #   Fs <- handel$Fs
  #   updateNumericInput(session, inputId = "samplingfreq", value = Fs)
  #   yfiltered <- signal::filter(filt=input$edit_gain*handlesb(), a=handlesa(), y)
  #   # yfiltered <- yfiltered / max(yfiltered) # normalization
  #   # plot(yfiltered)
  #   # print(head(yfiltered))
  #   yfiltered <- tuneR::normalize(as.vector(yfiltered), unit=1) # yfiltered/max(yfiltered) #   
  #   require(audio); try(audio::wait(audio::play(yfiltered, rate=Fs)),silent=TRUE)
  # })
  
  # observeEvents...: audiogeneratorsignal/ pb_generateaudio ----
  observeEvent(eventExpr = c(input$audiogeneratorsignal,
                             input$parameter1,
                             input$parameter2,
                             input$parameter3,
                             input$parameter4,
                             input$customsignalinput
                             , input$pb_generatecustom,
                             input$filenameAudio
                             # ,input$commonFilters
                             ,handles$poleloc,handles$zeroloc
                             ), 
               handlerExpr = {
    req(input$parameter1,
        input$parameter2,
        input$parameter3,
        input$parameter4
        # ,input$audiogeneratorsignal
        # ,handles$generatorWave
        )
    if (input$audiogeneratorsignal=="whitenoise") {
      # Fs <- 16000
      # nSecsDuration <- 4
      # nBits <- 16
      # stdv <- 0.196 # 10^(-1/sqrt(2)) # 10^(-7/10) # 0.2
      # handles$generatorWave <- (2^(nBits-1)-1) * rnorm(nSecsDuration*Fs, mean=0, sd=stdv)
      Fs <- 16000
      nSecsDuration <- 4
      nBits <- 16
      handles$generatorWave <- 
        # round(32767 * 
        tuneR::noise(
          kind="white", # c("white", "pink", "power", "red"), 
          duration=nSecsDuration*Fs, 
          samp.rate=Fs,
          bit=1, # numeric-values within the span of [-1,1] # 1 | 8 |  16 |  32 |  64 | 0 # 
          xunit="samples" # c("samples", "time")
        )
    } else if (input$audiogeneratorsignal=="pinknoise") {
      Fs <- 16000
      nSecsDuration <- 4
      nBits <- 16
      handles$generatorWave <- 
        # round(32767 * 
        tuneR::noise(
          kind="pink", # c("white", "pink", "power", "red"), 
          duration=nSecsDuration*Fs, 
          samp.rate=Fs,
          bit=1, # numeric-values within the span of [-1,1] # 1 | 8 |  16 |  32 |  64 | 0 # 
          xunit="samples" # c("samples", "time")
        )
    } else if (input$audiogeneratorsignal=="pulsed") {
      Fs <- 16000
      nSecsDuration <- 4
      nBits <- 16
      frq <- input$parameter1 # 2.5 # cycles-per-second, or Hz # 220 # Hertz
      
      if (input$parameter1 < 0.001) {updateNumericInput(session,inputId="parameter1",value=0.5);return()}
      if (input$parameter2 < 0) {updateNumericInput(session,inputId="parameter2",value=0);return()}
      if (input$parameter3 < 0) {updateNumericInput(session,inputId="parameter3",value=0);return()}
      if (input$parameter4 < 0) {updateNumericInput(session,inputId="parameter4",value=0);return()}
      if (input$parameter2 > 1) {updateNumericInput(session,inputId="parameter2",value=1);return()}
      if (input$parameter3 > 1) {updateNumericInput(session,inputId="parameter3",value=1);return()}
      if (input$parameter4 > 1) {updateNumericInput(session,inputId="parameter4",value=1);return()}
      
      handles$generatorWave <- 
        # round(32767 * 
        tuneR::pulse(
          freq=input$parameter1, # frq, 
          duration=nSecsDuration*Fs, 
          from=0, 
          samp.rate=Fs,
          bit=1, # numeric-values within the span of [-1,1] # 1 | 8 |  16 |  32 |  64 | 0 # 
          xunit="samples", # c("samples", "time"),
          width=input$parameter2, # 0.1, 
          plateau=input$parameter3, # 0.2, 
          interval=input$parameter4  # 0.5
        )
    } else if (input$audiogeneratorsignal=="sawtooth") {
      Fs <- 16000
      nSecsDuration <- 4
      nBits <- 16
      frq <- input$parameter1 # 2.5 # full-cycles over the entire signal-duration # 220 # Hertz
      handles$generatorWave <- 
        # round(32767 * 
        tuneR::sawtooth(
          freq=input$parameter1, # frq, 
          duration=nSecsDuration*Fs, 
          from=0, 
          samp.rate=Fs, 
          bit=1, # numeric-values within the span of [-1,1] # 1 | 8 |  16 |  32 |  64 | 0 # 
          xunit="samples", # c("samples", "time"), 
          reverse=FALSE
        )
    } else if (input$audiogeneratorsignal=="silence") {
      Fs <- 16000
      nSecsDuration <- 4
      nBits <- 16
      handles$generatorWave <- 
        # round(32767 * 
        tuneR::silence(
          duration=nSecsDuration*Fs,
          from=0, 
          samp.rate=Fs, 
          bit=1, # numeric-values within the span of [-1,1] # 1 | 8 |  16 |  32 |  64 | 0 # 
          xunit="samples" # c("samples", "time")
        )
    } else if (input$audiogeneratorsignal=="sinewave") {
      Fs <- 16000
      nSecsDuration <- 4
      nBits <- 16
      # fcutoff <- 0.01 # (about 220.5Hz, assuming Fs=44100)
      frq <- input$parameter1 # 2.5 # fcutoff*(Fs/2) # 220 # Hertz
      handles$generatorWave <- 
        # round(32767 * 
        tuneR::sine(
          freq=input$parameter1, # frq, 
          duration=nSecsDuration*Fs, 
          from=0, 
          samp.rate=Fs, 
          bit=1, # numeric-values within the span of [-1,1] # 1 | 8 |  16 |  32 |  64 | 0 #
          xunit="samples" # c("samples", "time")
        )
    } else if (input$audiogeneratorsignal=="squarewave") {
      Fs <- 16000
      nSecsDuration <- 4
      nBits <- 16
      frq <- input$parameter1 # 2.5 # full-cycles over the entire signal-duration # 220 # Hertz
      handles$generatorWave <- 
        # round(32767 * 
        tuneR::square(
          freq=input$parameter1, # frq, 
          duration=nSecsDuration*Fs, 
          from=0, 
          samp.rate=Fs, 
          bit=1, # numeric-values within the span of [-1,1] # 1 | 8 |  16 |  32 |  64 | 0 # 
          xunit="samples", # c("samples", "time"), 
          up=0.5
        )
    # } else if (input$audiogeneratorsignal=="heavisine") {
    } else if (input$audiogeneratorsignal %in% c("heavisine",
                              "bumps",
                              "blocks",
                              "doppler",
                              "ramp",
                              "cusp",
                              "sing",
                              "hisine",
                              "losine",
                              "linchirp",
                              "twochirp",
                              "quadchirp",
                              "mishmash",
                              "wernersorrows",
                              "leopold"
                              )
               ) {
      Fs <- 16000
      nSecsDuration <- 4
      sigsmtx <- rwt::makesig(SIGNAL.ALL, N=nSecsDuration*Fs)$x
      row.names(sigsmtx) <- c("heavisine",
                              "bumps",
                              "blocks",
                              "doppler",
                              "ramp",
                              "cusp",
                              "sing",
                              "hisine",
                              "losine",
                              "linchirp",
                              "twochirp",
                              "quadchirp",
                              "mishmash",
                              "wernersorrows",
                              "leopold"
                              )
      # rwtsgnl <- rwt::makesig(SIGNAL.HEAVI.SINE,N=nSecsDuration*Fs)$x
      rwtsgnl <- sigsmtx[input$audiogeneratorsignal,]
      # print(round(32767 * rwtsgnl/max(abs(rwtsgnl)))[1:100])
      handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl),na.rm=TRUE)),
                                                          ncol=1), # nrow=length(rwtsgnl)),
                                           samp.rate=Fs,
                                           bit=16,
                                           pcm=TRUE
                                           )
    # } else if (input$audiogeneratorsignal=="bumps") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.BUMPS,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="blocks") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.BLOCKS,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="doppler") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.DOPPLER,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="ramp") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.RAMP,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="cusp") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.CUSP,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="sing") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.SING,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="hisine") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.HI.SINE,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="losine") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.LO.SINE,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="linchirp") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.LIN.CHIRP,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="twochirp") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.TWO.CHIRP,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="quadchirp") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.QUAD.CHIRP,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="mishmash") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.MISH.MASH,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="wernersorrows") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.WERNER.SORROWS,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    # } else if (input$audiogeneratorsignal=="leopold") {
    #   Fs <- 16000
    #   nSecsDuration <- 4
    #   rwtsgnl <- rwt::makesig(SIGNAL.LEOPOLD,N=nSecsDuration*Fs)$x
    #   handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * rwtsgnl/max(abs(rwtsgnl)))),
    #                                        samp.rate=Fs,
    #                                        bit=16,
    #                                        pcm=TRUE
    #                                        )
    } else if (input$audiogeneratorsignal=="astsasoi") {
      data("soi", package="astsa", verbose=FALSE)
      Fs <- (length(astsa::soi)/0.3775)
      astsasgnl <- astsa::soi
      nSecsDuration <- length(astsasgnl)/Fs
      handles$generatorWave <- 
        tuneR::Wave(left=matrix(data=round(32767 * astsasgnl/max(abs(astsasgnl),na.rm=TRUE)),
                                                          ncol=1), # nrow=length(rwtsgnl)),
                                           samp.rate=Fs,
                                           bit=16,
                                           pcm=TRUE
                                           )
    } else if (input$audiogeneratorsignal=="astsacmort") {
      data("cmort", package="astsa", verbose=FALSE)
      Fs <- (length(astsa::cmort)/0.508)
      astsasgnl <- astsa::cmort
      nSecsDuration <- length(astsasgnl)/Fs
      handles$generatorWave <- 
        tuneR::Wave(left=matrix(data=round(32767 * astsasgnl/max(abs(astsasgnl),na.rm=TRUE)),
                                                          ncol=1), # nrow=length(rwtsgnl)),
                                           samp.rate=Fs,
                                           bit=16,
                                           pcm=TRUE
                                           )
    } else if (input$audiogeneratorsignal=="astsastar") {
      data("star", package="astsa", verbose=FALSE)
      Fs <- (length(astsa::star)/0.197)
      astsasgnl <- astsa::star
      nSecsDuration <- length(astsasgnl)/Fs
      handles$generatorWave <- 
        tuneR::Wave(left=matrix(data=round(32767 * astsasgnl/max(abs(astsasgnl),na.rm=TRUE)),
                                                          ncol=1), # nrow=length(rwtsgnl)),
                                           samp.rate=Fs,
                                           bit=16,
                                           pcm=TRUE
                                           )
    } else if (input$audiogeneratorsignal=="astsanyse") {
      data("nyse", package="astsa", verbose=FALSE)
      Fs <- (length(astsa::nyse)/0.794)
      astsasgnl <- astsa::nyse
      nSecsDuration <- length(astsasgnl)/Fs
      handles$generatorWave <- 
        tuneR::Wave(left=matrix(data=round(32767 * astsasgnl/max(abs(astsasgnl),na.rm=TRUE)),
                                                          ncol=1), # nrow=length(rwtsgnl)),
                                           samp.rate=Fs,
                                           bit=16,
                                           pcm=TRUE
                                           )
    } else if (input$audiogeneratorsignal=="sunspotmonthly") {
      data("sunspot.month", package="datasets", verbose=FALSE)
      Fs <- (length(datasets::sunspot.month)/0.265)
      datasetssgnl <- datasets::sunspot.month
      nSecsDuration <- length(datasetssgnl)/Fs
      handles$generatorWave <- 
        tuneR::Wave(left=matrix(data=round(32767 * datasetssgnl/max(abs(datasetssgnl),na.rm=TRUE)),
                                                          ncol=1), # nrow=length(rwtsgnl)),
                                           samp.rate=Fs,
                                           bit=16,
                                           pcm=TRUE
                                           )
    } else if (input$audiogeneratorsignal=="arimapdq") {
      Fs <- 4000
      N=1024
      b=c(-0.2279,0.2488)
      a=c(0.8897,-0.4858)
      Tseasonal=12
      arimapdqsgnl <- arima.sim(n=N,list(order=c(2,0,2),
                                         ma=b,ar=a
                                         ,seasonal=list(order=c(0,0,0),
                                                        period=Tseasonal)
                                         ),
                                rand.gen=rnorm,
                                sd=sqrt(0.1796)
                                )
      nSecsDuration <- length(arimapdqsgnl)/Fs
      handles$generatorWave <- 
        tuneR::Wave(left=matrix(data=round(32767 * arimapdqsgnl/max(abs(arimapdqsgnl),na.rm=TRUE)),
                                                          ncol=1), # nrow=length(rwtsgnl)),
                                           samp.rate=Fs,
                                           bit=16,
                                           pcm=TRUE
                                           )
    } else if (input$audiogeneratorsignal=='custom') {
      if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_customsignalText,ignore.case=FALSE))) return() # info-system security
      xFsList <- try(eval(parse(text=input$edit_customsignalText)),silent=TRUE)
      if (!is.list(xFsList)) return()
      xcustom <- matrix(xFsList$xn,ncol=1) # nrow=length(xFsList$xn))
      # print(str(xcustom))
      # print(str(matrix(round(32767 * xcustom/max(abs(xcustom),na.rm=TRUE)),ncol=1))) # ,nrow=length(xcustom))
      Fscustom <- xFsList$Fs
      handles$generatorWave <- tuneR::Wave(left=matrix(data=round(32767 * xcustom/max(abs(xcustom),na.rm=TRUE)),
                                                          ncol=1), # nrow=length(xcustom),
                                           samp.rate=Fscustom,
                                           bit=16,
                                           pcm=TRUE
                                           )
    }
      
      # cat(file=stderr(),"L3698",".\n")
      if (!isS4(handles$generatorWave)) return()
      if (is.null(input$audiogeneratorsignal)) return()
      # print(handles$generatorWave)
      # print(str(handles$generatorWave))
      # print(str(handles$generatorWave@left))
      # print(min(handles$generatorWave@left))
      # print(max(handles$generatorWave@left,na.rm=TRUE))

    # if (!is.null(input$audiogeneratorsignal) && isS4(handles$generatorWave)) {
      # print(handles$generatorWave)
      # print(input$audiogeneratorsignal)
      ygen <- handles$generatorWave@left
      objWave <- tuneR::Wave(left=matrix(data=round(32767 * ygen/max(abs(ygen),na.rm=TRUE)),
                                          ncol=1), # nrow=length(ygen)),
                           samp.rate=Fs,
                           bit=16,
                           pcm=TRUE
                           )
      # FundFreq <- tuneR::FF(tuneR::periodogram(objWave))
      tdir <- "www"
      tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$audiogeneratorsignal),".wav"))
      # cat(file=stderr(),"L3786 input$audiogeneratorsignal:",input$audiogeneratorsignal,".\n")
      if (!file.exists(tfile)
          # || (input$audiogeneratorsignal=="custom")
          ) {
        try(tuneR::writeWave(objWave, tfile),silent=TRUE)
      }
    # }
  })
  
  # observeEvent(eventExpr = input$filenameAudio, handlerExpr = {
  #   tuneR::writeWave
  # })
  
  # observeEvent pb_playfile ----
  observeEvent(eventExpr = input$pb_playfile, # c(input$pb_playfile,
  #                            input$filenameAudio
  #                            ), 
  handlerExpr = {
    if (input$inputsignalsource=="file") {
      # require(tuneR)
      #tuneR::setWavPlayer('"C:/Program Files/Windows Media Player/wmplayer.exe"')
      #[x,Fs,nBits]= wavread("test");
      # cat(file=stderr(),"input$filenameAudio$name:",input$filenameAudio$name,".\n")
      if (is.null(input$filenameAudio$name) || (!nzchar(input$filenameAudio$name, keepNA = FALSE))) {return()}
      if (tools::file_ext(input$filenameAudio$name) == "wav") {
        # objWav <- try(tuneR::readWave(input$filenameAudio$name),silent=TRUE)
        objWav <- try(tuneR::readWave(input$filenameAudio$name),silent=TRUE)
        if (isS4(objWav)) {
          # print(objWav)
        # a <- c(rv$samples, rv$channels)
        # numOfSamples= a[1]
        # nChannels= a[2]
        y <- objWav@left # assumed monophonic, data stored within left-channel only
        #if (objWav@stereo) {xright <- objWav@right} # if stereo-recording
        Fs <- objWav@samp.rate
        # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
        # isPCM <- objWav@pcm
        nBits <- objWav@bit
        # SIZEwav <- length(y)
        # FundFreq <- tuneR::FF(tuneR::periodogram(objWav))
        #if (interactive()) tuneR::play(objWav)
        # Sys.sleep(1)
        }
      } else if (tools::file_ext(input$filenameAudio$name) == "mp3") {
        objMP3 <- try(tuneR::readMP3(input$filenameAudio$name),silent=TRUE)
        if (isS4(objMP3)) {
        # a <- c(rv$samples, rv$channels)
        # numOfSamples= a[1]
        # nChannels= a[2]
        y <- objMP3@left # assumed monophonic, data stored within left-channel only
        #if (objMP3@stereo) {xright <- objMP3@right} # if stereo-recording
        Fs <- objMP3@samp.rate
        # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
        # isPCM <- objMP3@pcm
        nBits <- objMP3@bit
        # SIZEwav <- length(y)
        
        Wobj <- tuneR::Wave(left=matrix(data=round(32767 * y/max(abs(y),na.rm=TRUE)),
                                           ncol=1), 
                            samp.rate = as.numeric(Fs), 
                            bit = 16, 
                            pcm = TRUE)
        # FundFreq <- tuneR::FF(tuneR::periodogram(Wobj))
        # print(Wobj)
        # tdir <- tempdir()
        tdir <- "www"
        tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav"))
        # tfile <- paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")
        # tfile <- input$filenameAudio$name
        cat(file=stderr(),"L3746 tfile:",tfile,".\n")
        handles$tempfilteredfilelocation <- tfile
        
        if (!file.exists(tfile)
            # || (input$audiogeneratorsignal=="custom")
            ) {
          try(tuneR::writeWave(Wobj, filename = tfile),silent=TRUE) # input$filenameAudio$name) # 
        }
        # close(file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")))
        Sys.sleep(5) # wait 5 seconds
    
        # print(list.files(tdir, pattern = "\\.wav$"))
        newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)

        }
      } else if (tools::file_ext(input$filenameAudio$name) == "mat") {
        matmusic <- R.matlab::readMat(input$filenameAudio$name) # y= audio-signal, and Fs=8192 samples-per-second
        if (!is.list(matmusic)) return()
        y <- matmusic$y
        print(str(y))
        Fs <- as.numeric(matmusic$Fs)
        nBits <- 16 # assumed!?!?
        # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
        
        Wobj <- tuneR::Wave(left=matrix(data=round(32767 * y/max(abs(y),na.rm=TRUE)),
                                           ncol=1), 
                            samp.rate = as.numeric(Fs), 
                            bit = 16, 
                            pcm = TRUE)
        # print(Wobj)
        # tdir <- tempdir()
        tdir <- "www"
        tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav"))
        # tfile <- paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")
        # tfile <- input$filenameAudio$name
        cat(file=stderr(),"L3771 tfile:",tfile,".\n")
        handles$tempfilteredfilelocation <- tfile
        
        if (!file.exists(tfile) 
            # || (input$audiogeneratorsignal=="custom")
            ) {
          try(tuneR::writeWave(Wobj, filename = tfile),silent=TRUE) # input$filenameAudio$name) # 
        }
        # close(file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")))
        Sys.sleep(5) # wait 5 seconds
    
        # print(list.files(tdir, pattern = "\\.wav$"))
        newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)

      } else {return()}
    } else { # use generator
        objWav <- handles$generatorWave
        if (!isS4(objWav)) return()
        y <- objWav@left # assumed monophonic, data stored within left-channel only
        #if (objWav@stereo) {xright <- objWav@right} # if stereo-recording
        Fs <- objWav@samp.rate
        # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
        # isPCM <- objWav@pcm
        nBits <- objWav@bit
        # FundFreq <- tuneR::FF(tuneR::periodogram(objWav))
    }
    
    if (!exists("y")) {return()}
    yfiltered <- try(signal::filter(filt=input$edit_gain*handlesb(),
                                    a=handlesa(), 
                                    y
                                    ),silent=TRUE)
    # yfiltered <- yfiltered / max(yfiltered,na.rm=TRUE) # normalization
    # plot(yfiltered)
    # print(head(yfiltered))
    # cat(file=stderr(),"L3261 head(yfiltered), before normalization:",head(yfiltered),".\n")
    # yfiltered <- tuneR::normalize(as.vector(yfiltered), unit=1) # yfiltered/max(yfiltered) # 
    
    # Wobj <- tuneR::Wave(left=matrix(data=round(32767 * yfiltered/max(abs(yfiltered)))), samp.rate = as.numeric(Fs), bit = 16, pcm = TRUE)
    # # print(Wobj)
    # # tdir <- tempdir()
    # tdir <- "www"
    # tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),"filtered.wav")) # tools::file_path_sans_ext(input$filenameAudio$name),"filtered.wav") # input$filenameAudio$name))
    # cat(file=stderr(),"tfile:",tfile,".\n")
    # # tfile <- paste0("filtered",input$filenameAudio$name)
    # cat(file=stderr(),"L3809 tfile:",tfile,".\n")
    # handles$tempfilteredfilelocation <- tfile
    # 
    # # if (!file.exists(tfile) || input$audiogeneratorsignal=="custom") 
    #   tuneR::writeWave(Wobj, filename = tfile) # paste0("filtered",input$filenameAudio$name)) # 
    # # close(file.path(tdir, paste0("filtered",input$filenameAudio$name)))
    # Sys.sleep(5) # wait 5 seconds
    # 
    # # print(list.files(tdir, pattern = "\\.wav$"))
    # newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)
    # # print(newWobjList)
    # 
    # output$HTML5audioWidget2 <- renderUI({
    #   # if (is.null(input$filenameAudio$name) || (!nzchar(input$filenameAudio$name, keepNA = FALSE))) {return()}
    #   # if ((tools::file_ext(input$filenameAudio$name) == "wav") # || (tools::file_ext(input$filenameAudio$name) == "mp3")
    #   # ) {
    #   # https://groups.google.com/forum/#!topic/shiny-discuss/zO8hEFCxa0c; Andrew Caines, 19/09/2014
    #   p("Filtered: ",
    #   tags$audio(src = handles$tempfilteredfilelocation, type = "audio/wav", controls = TRUE, preload="metadata")
    #   )
    #     # HTML(paste0('Filtered: <audio controls="controls" preload="metadata"> ',
    #     #             '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/wav"> ', # input$filenameAudio$datapath),' type="audio/wav"> ',
    #     #             # '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/wave"> ',
    #     #             # '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/ogg"> ',
    #     #             # '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/mpeg"> ',
    #     #             'Please Note: Your browser does not support HTML5 digital-audio playback. ',
    #     #             'Please upgrade to the latest version of your browser or operating system. ',
    #     #             '</audio>'
    #     #     )
    #     # )
    #   # }
    # })

    # cat(file=stderr(),"L3273 head(yfiltered):",head(yfiltered),".\n")
    # cat(file=stderr(),"max(yfiltered):",max(yfiltered),".\n")
    # cat(file=stderr(),"min(yfiltered):",min(yfiltered),".\n")
    # cat(file=stderr(),"Fs:",Fs,".\n")

    if (!exists("y")) {return()}
    y <- y/max(abs(y),na.rm=TRUE) * 1.0 #  tuneR::normalize(as.vector(y),unit=1) #   
    # cat(file=stderr(),"head(y):",head(y),".\n")
    # cat(file=stderr(),"max(y):",max(y),".\n")
    # cat(file=stderr(),"min(y):",min(y),".\n")
    # cat(file=stderr(),"Fs:",Fs,".\n")
    if ("windows" %in% get_os()) {
      showNotification(
        ui = paste0("Now playing the original y signal, '",input$filenameAudio$name,"' (",Fs," Samples/s) ..."),
        duration = NULL, # 3,
        id="nowplaying",
        closeButton = TRUE,
        type = "message"
      )
      require(audio); try(audio::wait(audio::play(y, rate=Fs)),silent=TRUE)
      removeNotification(id="nowplaying",session)

    }
  })
  
    # output$filenameAudioInfo Print ----
  output$filenameAudioInfo <- renderText({
    if (input$inputsignalsource=="file") {
    if (is.null(input$filenameAudio$name) || (!nzchar(input$filenameAudio$name, keepNA = FALSE))) {return()}
    
    {
    if (tools::file_ext(input$filenameAudio$name) == "wav") {
      # objWav <- try(tuneR::readWave(input$filenameAudio$name),silent=TRUE)
      objWav <- try(tuneR::readWave(input$filenameAudio$name),silent=TRUE)
      if (isS4(objWav)) {
        # print(objWav)
      # a <- c(rv$samples, rv$channels)
      # numOfSamples= a[1]
      # nChannels= a[2]
      y <- objWav@left # assumed monophonic, data stored within left-channel only
      #if (objWav@stereo) {xright <- objWav@right} # if stereo-recording
      Fs <- objWav@samp.rate
      # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
      # isPCM <- objWav@pcm
      nBits <- objWav@bit
      # SIZEwav <- length(y)
      #if (interactive()) tuneR::play(objWav)
      # Sys.sleep(1)
      # FundFreq <- tuneR::FF(tuneR::periodogram(objWav))
      }
    } else if (tools::file_ext(input$filenameAudio$name) == "mp3") {
      objMP3 <- try(tuneR::readMP3(input$filenameAudio$name),silent=TRUE)
      if (isS4(objMP3)) {
      # a <- c(rv$samples, rv$channels)
      # numOfSamples= a[1]
      # nChannels= a[2]
      y <- objMP3@left # assumed monophonic, data stored within left-channel only
      #if (objMP3@stereo) {xright <- objMP3@right} # if stereo-recording
      Fs <- objMP3@samp.rate
      # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
      # isPCM <- objMP3@pcm
      nBits <- objMP3@bit
      # SIZEwav <- length(y)
      
      Wobj <- tuneR::Wave(left=matrix(data=round(32767 * y/max(abs(y),na.rm=TRUE)),
                                         ncol=1), 
                          samp.rate = as.numeric(Fs), 
                          bit = 16, 
                          pcm = TRUE)
      # FundFreq <- tuneR::FF(tuneR::periodogram(Wobj))
      # print(Wobj)
      # tdir <- tempdir()
      tdir <- "www"
      tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav"))
      # tfile <- paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")
      # tfile <- input$filenameAudio$name
      # cat(file=stderr(),"L3899 tfile:",tfile,".\n")
      handles$tempfilteredfilelocation <- tfile
      
      if (!file.exists(tfile) 
          # || (input$audiogeneratorsignal=="custom")
          ) {
        try(tuneR::writeWave(Wobj, 
                             filename = tfile
                             ),silent=TRUE) # input$filenameAudio$name) # 
      }
      # close(file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")))
      Sys.sleep(5) # wait 5 seconds
  
      # print(list.files(tdir, pattern = "\\.wav$"))
      newWobjList <- try(tuneR::readWave(tfile,
                                         header = TRUE
                                         ),silent=TRUE)
      }
    } else if (tools::file_ext(input$filenameAudio$name) == "mat") {
      matmusic <- R.matlab::readMat(input$filenameAudio$name) # y= audio-signal, and Fs=8192 samples-per-second
      if (!is.list(matmusic)) return()
      y <- matmusic$y
      print(str(y))
      Fs <- as.numeric(matmusic$Fs)
      nBits <- 16 # assumed!?!?

      # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
      
      Wobj <- try(tuneR::Wave(left=matrix(data=round(32767 * y/max(abs(y),na.rm=TRUE)),
                                         ncol=1), 
                          samp.rate = as.numeric(Fs), 
                          bit = 16, 
                          pcm = TRUE
                          ),silent=TRUE)
      # FundFreq <- tuneR::FF(tuneR::periodogram(Wobj))
      # print(Wobj)
      # tdir <- tempdir()
      tdir <- "www"
      tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav"))
      # tfile <- paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")
      # tfile <- input$filenameAudio$name
      # cat(file=stderr(),"L3924 tfile:",tfile,".\n")
      handles$tempfilteredfilelocation <- tfile
      
      if (!file.exists(tfile) 
          # || (input$audiogeneratorsignal=="custom")
          ) {
        try(tuneR::writeWave(Wobj, filename = tfile),silent=TRUE) # input$filenameAudio$name) # 
      }
      # close(file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")))
      Sys.sleep(5) # wait 5 seconds
  
      # print(list.files(tdir, pattern = "\\.wav$"))
      newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)

    } else {return()}

    yfiltered <- try(signal::filter(filt=input$edit_gain*handlesb(), 
                                    a=handlesa(), 
                                    y
                                    ),silent=TRUE)
    # yfiltered <- yfiltered / max(yfiltered) # normalization
    # plot(yfiltered)
    # cat(file=stderr(),"L3334 head(yfiltered), before normalization:",head(yfiltered),".\n")
    # yfiltered <- tuneR::normalize(as.vector(yfiltered), unit=1) # yfiltered/max(yfiltered) # 
    
  # cat(file=stderr(),"L3337 head(yfiltered):",head(yfiltered),".\n")
    # cat(file=stderr(),"max(yfiltered):",max(yfiltered),".\n")
    # cat(file=stderr(),"min(yfiltered):",min(yfiltered),".\n")
    # cat(file=stderr(),"Fs:",Fs,".\n")
    # 
    # cat(file=stderr(),"L3348 head(yfiltered...), normalized:",head(32767 * yfiltered/max(abs(yfiltered))),".\n")
    # cat(file=stderr(),"max(yfiltered...):",max(32767 * yfiltered/max(abs(yfiltered))),".\n")
    # cat(file=stderr(),"min(yfiltered...):",min(32767 * yfiltered/max(abs(yfiltered))),".\n")

    Wobj <- try(tuneR::Wave(left=matrix(data=round(32767 * yfiltered/max(abs(yfiltered),na.rm=TRUE)),
                                       ncol=1), # nrow=length(yfiltered)), 
                        samp.rate = as.numeric(Fs), 
                        bit = 16, 
                        pcm = TRUE),silent=TRUE)
    # FundFreq <- tuneR::FF(tuneR::periodogram(Wobj))
    # print(Wobj)
    # tdir <- tempdir()
    tdir <- "www"
    tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),"filtered.wav")) # tools::file_path_sans_ext(input$filenameAudio$name),"filtered.wav") # input$filenameAudio$name))
    # tfile <- file.path(paste0(tools::file_path_sans_ext(input$filenameAudio$name),"filtered.wav"))
    # tfile <- paste0("filtered",input$filenameAudio$name)
    # cat(file=stderr(),"L3963 tfile:",tfile,".\n")
    handles$tempfilteredfilelocation <- paste0(tools::file_path_sans_ext(input$filenameAudio$name),"filtered.wav")
    
    # if (!file.exists(tfile) || input$audiogeneratorsignal=="custom") 
      try(tuneR::writeWave(Wobj, 
                           filename = tfile
                           ),silent=TRUE) # paste0("filtered",input$filenameAudio$name)) # 
    # close(file.path(tdir, paste0("filtered",input$filenameAudio$name)))
    Sys.sleep(5) # wait 5 seconds

    # print(list.files(tdir, pattern = "\\.wav$"))
    newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)
    # print(newWobjList)
   
    # output$HTML5audioWidget2 <- renderUI({
    #   # if (is.null(input$filenameAudio$name) || (!nzchar(input$filenameAudio$name, keepNA = FALSE))) {return()}
    #   # if ((tools::file_ext(input$filenameAudio$name) == "wav") # || (tools::file_ext(input$filenameAudio$name) == "mp3")
    #   # ) {
    #   # https://groups.google.com/forum/#!topic/shiny-discuss/zO8hEFCxa0c; Andrew Caines, 19/09/2014
    #   if (input$inputsignalsource=="file") {
    #     p("Filtered: ",
    #     tags$audio(src = paste0(tools::file_path_sans_ext(input$filenameAudio$name),"filtered.wav"), type = "audio/wav", controls = TRUE, preload="metadata")
    #     )
    #   } else if (input$inputsignalsource=="generator") {
    #     tags$audio(src = paste0(input$audiogeneratorsignal,"filtered.wav"), type = "audio/wav", controls = TRUE, preload="metadata")
    #   } 
    #   
    #     # HTML(paste0('Filtered: <audio controls="controls" preload="metadata"> ',
    #     #             '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/wav"> ', # input$filenameAudio$datapath),' type="audio/wav"> ',
    #     #             # '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/wave"> ',
    #     #             # '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/ogg"> ',
    #     #             # '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/mpeg"> ',
    #     #             'Please Note: Your browser does not support HTML5 digital-audio playback. ',
    #     #             'Please upgrade to the latest version of your browser or operating system. ',
    #     #             '</audio>'
    #     #     )
    #     # )
    #   # }
    # })
    # # Wobj <- tuneR::Wave(left=tuneR::normalize(as.vector(trunc(yfiltered)),unit=16), samp.rate = Fs, bit = 16, pcm = TRUE)
    # Wobj <- tuneR::Wave(left=matrix(data=round(32767 * yfiltered/max(abs(yfiltered)))), samp.rate = Fs, bit = 16, pcm = TRUE)
    # # print(Wobj)
    # # Wobj <- tuneR::Wave(left=t(t(yfiltered)), samp.rate = Fs, bit = 16, pcm = TRUE)
    # tdir <- tempdir()
    # tdir <- "www"
    # tfile <- file.path(tdir, paste0("filtered",input$filenameAudio$name))
    # # tfile <- paste0("filtered",input$filenameAudio$name)
    # handles$tempfilteredfilelocation <- tfile

    # if (!file.exists(tfile) || input$audiogeneratorsignal=="custom") 
    #    tuneR::writeWave(Wobj, filename = tfile) # paste0("filtered",input$filenameAudio$name)) # 
    # close(file.path(tdir, paste0("filtered",input$filenameAudio$name)))
    # Sys.sleep(5) # wait 5 seconds
    
    # print(list.files(tdir, pattern = "\\.wav$"))
    # newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)
    # print(newWobjList)
    }
    
    if (tools::file_ext(input$filenameAudio$name) == "wav") {
      objWavList <- try(tuneR::readWave(input$filenameAudio$name, 
                                    header = TRUE),silent=TRUE)
      objWav <- try(tuneR::readWave(input$filenameAudio$name),silent=TRUE)
      # print(objWav)
      # a <- c(, )
      if (is.list(objWavList)) {
      paste(input$filenameAudio$name, "\n",
            "number of Samples:", objWavList$samples, "\n",
            "Duration (secs):",round(objWavList$samples/objWavList$sample.rate,1),"\n",
            "nBits resolution:", objWavList$bits, "\n",
            "nChannels:", objWavList$channels, "\n",
            "Fs, sample-rate:", objWavList$sample.rate, "\n",
            "Ts, sample-period (msecs):", round(1000/objWavList$sample.rate,3), "\n",
            "(~50msecs delay, for echo-effects):", round(0.050*objWavList$sample.rate,1)
            ,"\n","maxvalue:",max(objWav@left,na.rm=TRUE)
            ,"\n","minvalue:",min(objWav@left,na.rm=TRUE)
            # ,"\n","Fund.Freq.:",try(tuneR::FF(tuneR::periodogram(objWav)),silent=TRUE)
            )
      }
    } else if (tools::file_ext(input$filenameAudio$name) == "mp3") {
      objMP3 <- try(tuneR::readMP3(input$filenameAudio$name),silent=TRUE)
      if (isS4(objMP3)) {
      # print(objMP3)
      # a <- c(, )
      paste(input$filenameAudio$name, "\n",
            "number of Samples (left-channel):", length(objMP3@left), "\n",
            "Duration (secs):",round(length(objMP3@left)/objMP3@samp.rate,1),"\n",
            "nBits resolution:", objMP3@bit, "\n",
            "nChannels:", 2, "\n",
            "Fs, sample-rate:", objMP3@samp.rate, "\n",
            "Ts, sample-period (msecs):", round(1000/objMP3@samp.rate,3), "\n",
            "(~50msecs delay, for echo-effects):", round(0.050*objMP3@samp.rate,1), "\n",
            "maxvalue:",max(objMP3@left,na.rm=TRUE),"\n",
            "minvalue:",min(objMP3@left,na.rm=TRUE)
            )
      }
    } else if (tools::file_ext(input$filenameAudio$name) == "mat") {
      matmusic <- try(R.matlab::readMat(input$filenameAudio$name, verbose=TRUE),silent=TRUE) # y= audio-signal, and Fs=8192 samples-per-second
      if (is.list(matmusic)) {
        # y <- matmusic$y
        paste(input$filenameAudio$name, "\n",
              "length:", length(matmusic$y), "\n",
              "Duration (secs):",round(length(matmusic$y)/matmusic$Fs,1),"\n",
              "Fs, sample-rate:", matmusic$Fs, "\n",
              "Ts, sample-period (msecs):", round(1000/matmusic$Fs,3), "\n",
              "(~50msecs delay, for echo-effects):", round(0.050*matmusic$Fs,1), "\n",
              "maxvalue:",max(matmusic$y,na.rm=TRUE),"\n",
              "minvalue:",min(matmusic$y,na.rm=TRUE)
              )
      }
    } else {return()}
    } else { # using generator
      if (input$audiogeneratorsignal %in% c("whitenoise",
                                            "pinknoise",
                                            "pulsed",
                                            "sawtooth",
                                            # "silence",
                                            "sinewave",
                                            "squarewave",
                                            "heavisine",
                                            "bumps",
                                            "blocks",
                                            "doppler",
                                            "ramp",
                                            "cusp",
                                            "sing",
                                            "hisine",
                                            "losine",
                                            "linchirp",
                                            "twochirp",
                                            "quadchirp",
                                            "mishmash",
                                            "wernersorrows",
                                            "leopold"
                                            ,"astsasoi"
                                            ,"astsastar"
                                            ,"astsacmort"
                                            ,"astsanyse"
                                            ,"sunspotmonthly"
                                            ,"arimapdq"
                                            ,"custom"
                                            )
          ) {
        # Fs <- 16000
        frq <- input$parameter1 # 2.5 # 220
        # nSecsDuration <- 4
        # nBits <- 16
        # stdv <- 0.196 # 10^(-1/sqrt(2)) # 10^(-7/10) # 0.2
        # y <- handles$generatorWave # (2^(nBits-1)-1) * rnorm(nSecsDuration*Fs, mean=0, sd=stdv)
        objWav <- handles$generatorWave
        if (!isS4(objWav)) return()
        y <- objWav@left # assumed monophonic, data stored within left-channel only
        #if (objWav@stereo) {xright <- objWav@right} # if stereo-recording
        Fs <- objWav@samp.rate
        # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
        # isPCM <- objWav@pcm
        nBits <- objWav@bit
        # FundFreq <- tuneR::FF(tuneR::periodogram(objWav))        
      # } else if (input$audiogeneratorsignal %in% c(
      #                                       )
      #     ) {
      # } else if (input$audiogeneratorsignal=='custom') {
      #   # TBD
      }
              paste("(generator):",input$audiogeneratorsignal, "\n",
              "length:", length(y), "\n",
              "Duration (secs):",round(length(y)/Fs,1),"\n",
              "Fs, sample-rate:", Fs, "\n",
              "Ts, sample-period (msecs):", round(1000/Fs,3), "\n",
              "(~50msecs delay, for echo-effects):", round(0.050*Fs,1), "\n",
              "maxvalue:",max(y,na.rm=TRUE),"\n",
              "minvalue:",min(y,na.rm=TRUE)
              ,if (input$audiogeneratorsignal %in% c("sinewave","pulsed") 
                   ) {paste0("\n","freq (Hz):",input$parameter1)} # 220)}
              ,if (input$audiogeneratorsignal %in% c("sawtooth","squarewave") 
                   ) {paste0("\n","freq (cycles):",input$parameter1)} # 220)}
              ,if (input$audiogeneratorsignal %in% c("pulsed") ) {
                paste0("\n","pulse-width (%):",input$parameter2 *100, 
                       "\n","plateau (%):",input$parameter3 *100, 
                       "\n","interval (%):",input$parameter4 *100)
              }
              ,if (input$audiogeneratorsignal %in% c("squarewave") ) {
                paste0("\n","duty-cycle (%):",0.5 *100)
              }
              # ,"\n","Fund.Freq.:",try(tuneR::FF(tuneR::periodogram(objWav)),silent=TRUE)

              )
    }
  })
  
  output$HTML5audioWidget <- renderUI({
    # if (( is.null(input$filenameAudio$name) ) || 
    #     ( !nzchar(input$filenameAudio$name, keepNA = FALSE) )
    # ) {
    #   return()
    # }
    # if ((tools::file_ext(input$filenameAudio$name) == "wav") # || (tools::file_ext(input$filenameAudio$name) == "mp3")
    # ) {
#         HTML(paste0('Original:',
# '<audio controls="controls" preload="metadata"> 
#    <source src="', input$filenameAudio$name, '" type="audio/wav">
# Please Note: Your browser does not support HTML5 digital-audio playback.  Please upgrade to the latest version of your browser or operating system.
# </audio>'
#          ))
    if (input$inputsignalsource=="file") {
      p("Original: ",
      tags$audio(src = paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav"), type = "audio/wav", controls = TRUE, preload="metadata")
      )
    } else if (input$inputsignalsource=="generator") {
      tags$audio(src = paste0(input$audiogeneratorsignal,".wav"), type = "audio/wav", controls = TRUE, preload="metadata")
    } 

#         p('Original:',
# '<audio controls="controls" preload="metadata"> 
#    <source src="', input$filenameAudio$name, '" type="audio/wav">
# Please Note: Your browser does not support HTML5 digital-audio playback.  Please upgrade to the latest version of your browser or operating system.
# </audio>'
#           )
        # HTML('<audio src="', input$filenameAudio$name, '" controls>')
                    # '<source src="',input$filenameAudio$name,'" type="audio/wave"> ',
                    # '<source src="',input$filenameAudio$name,'" type="audio/ogg"> ',
                    # '<source src="',input$filenameAudio$name,'" type="audio/mpeg"> ',
        # HTML(paste0('<div title="Original2">Original2<audio controls="controls" preload="none"> ',
        #             '<source src=',input$filenameAudio$datapath,' type="audio/wav"> ', # input$filenameAudio$datapath,' type="audio/wav"> ',
        #             '<source src=',input$filenameAudio$datapath,' type="audio/ogg"> ',
        #             '<source src=',input$filenameAudio$datapath,' type="audio/mpeg"> ',
        #             'Please Note: Your browser does not support HTML5 digital-audio playback. ',
        #             'Please upgrade to the latest version of your browser or operating system. ',
        #             '</audio></div>'
        #   ))
        # HTML(paste0('<div><audio controls="controls"> ',
        #             '<source src=',input$filenameAudio$name,' type="audio/wav"> ', # input$filenameAudio$datapath,' type="audio/wav"> ',
        #             '<source src=',input$filenameAudio$name,' type="audio/ogg"> ',
        #             '<source src=',input$filenameAudio$name,' type="audio/mpeg"> ',
        #             'Please Note: Your browser does not support HTML5 digital-audio playback. ',
        #             'Please upgrade to the latest version of your browser or operating system. ',
        #             '</audio></div>'
        #   ))
        # HTML(paste0('<div><audio controls="controls"> ',
        #             '<source src=',input$filenameAudio$datapath,' type="audio/wav"> ', # input$filenameAudio$datapath,' type="audio/wav"> ',
        #             '<source src=',input$filenameAudio$datapath,' type="audio/ogg"> ',
        #             '<source src=',input$filenameAudio$datapath,' type="audio/mpeg"> ',
        #             'Please Note: Your browser does not support HTML5 digital-audio playback. ',
        #             'Please upgrade to the latest version of your browser or operating system. ',
        #             '</audio></div>'
        #   ))
      # HTML(paste0('<div><audio controls> <source src=',
      #             input$filenameAudio$name,
      #             ' type="audio/wave"> Please Note: Your browser does not support HTML5 digital-audio playback!  Please upgrade to the latest version of your browser or operating system. </audio></div>'
      #     ))
      # HTML(paste0('<audio src="',
      #             input$filenameAudio$datapath,
      #             '" controls>'
      #     ))
      # HTML(paste0('<audio src="',
      #             input$filenameAudio$name,
      #             '" controls>'
      #     ))
    # }
  })
  
  # observeEvents...: pb_playFiltered, etc ----
  observeEvent(eventExpr = c(input$pb_playFiltered,
                             input$filenameAudio,
                             input$audiogeneratorsignal
                             # input$commonFilters
                             ,handles$poleloc,handles$zeroloc
                             ), handlerExpr = {
    # require(tuneR)
    #tuneR::setWavPlayer('"C:/Program Files/Windows Media Player/wmplayer.exe"')
    #[x,Fs,nBits]= wavread("test");
    # cat(file=stderr(),"input$filenameAudio$name:",input$filenameAudio$name,".\n")
    if (input$inputsignalsource=="file") { 
    if (is.null(input$filenameAudio$name) || (!nzchar(input$filenameAudio$name, keepNA = FALSE))) {return()}
    if (tools::file_ext(input$filenameAudio$name) == "wav") {
      objWav <- try(tuneR::readWave(input$filenameAudio$name),silent=TRUE)
      if (isS4(objWav)) {
        # print(objWav)
      y <- objWav@left # assumed monophonic, data stored within left-channel only
      #if (objWav@stereo) {xright <- objWav@right} # if stereo-recording
      Fs <- objWav@samp.rate
      # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
      nBits <- objWav@bit
      # SIZEwav <- length(y)
      #if (interactive()) tuneR::play(objWav)
      # Sys.sleep(1)
      # FundFreq <- tuneR::FF(tuneR::periodogram(objWav))
      }
    } else if (tools::file_ext(input$filenameAudio$name) == "mp3") {
      objMP3 <- try(tuneR::readMP3(input$filenameAudio$name),silent=TRUE)
      if (isS4(objMP3)) {
      y <- objMP3@left # assumed monophonic, data stored within left-channel only
      #if (objMP3@stereo) {xright <- objMP3@right} # if stereo-recording
      Fs <- objMP3@samp.rate
      # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
      nBits <- objMP3@bit
      # SIZEwav <- length(y)
      #if (interactive()) tuneR::play(objMP3)
      # Sys.sleep(1)
      
      Wobj <- tuneR::Wave(left=matrix(data=round(32767 * y/max(abs(y),na.rm=TRUE)),
                                         ncol=1), 
                          samp.rate = as.numeric(Fs), 
                          bit = 16, 
                          pcm = TRUE)
      # print(Wobj)
      # tdir <- tempdir()
      tdir <- "www"
      tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav"))
      # tfile <- paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")
      # tfile <- input$filenameAudio$name
      cat(file=stderr(),"L4199 tfile:",tfile,".\n")
      handles$tempfilteredfilelocation <- tfile
      
      if (!file.exists(tfile) 
          # || (input$audiogeneratorsignal=="custom")
          ) {
        try(tuneR::writeWave(Wobj, filename = tfile),silent=TRUE) # input$filenameAudio$name) # 
      }
      # close(file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")))
      Sys.sleep(5) # wait 5 seconds
  
      # print(list.files(tdir, pattern = "\\.wav$"))
      newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)

      }
    } else if (tools::file_ext(input$filenameAudio$name) == "mat") {
      matmusic <- R.matlab::readMat(input$filenameAudio$name) # y= audio-signal, and Fs=8192 samples-per-second
      if (!is.list(matmusic)) return()
      y <- matmusic$y
      print(str(y))
      Fs <- as.numeric(matmusic$Fs)
      nBits <- 16 # assumed!?!?

      # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
      
      Wobj <- tuneR::Wave(left=matrix(data=round(32767 * y/max(abs(y),na.rm=TRUE)),
                                         ncol=1), 
                          samp.rate = as.numeric(Fs), 
                          bit = 16, 
                          pcm = TRUE)
      # print(Wobj)
      # tdir <- tempdir()
      tdir <- "www"
      tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav"))
      # tfile <- paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")
      # tfile <- input$filenameAudio$name
      cat(file=stderr(),"L4225 tfile:",tfile,".\n")
      handles$tempfilteredfilelocation <- tfile
      
      if (!file.exists(tfile) 
          # || (input$audiogeneratorsignal=="custom")
          ) {
        try(tuneR::writeWave(Wobj, filename = tfile),silent=TRUE) # input$filenameAudio$name) # 
      }
      # close(file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")))
      Sys.sleep(5) # wait 5 seconds
  
      # print(list.files(tdir, pattern = "\\.wav$"))
      newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)

    } else {return()}
    } else { # using generator
      if (input$audiogeneratorsignal %in% c("whitenoise",
                                            "pinknoise",
                                            "pulsed",
                                            "sawtooth",
                                            # "silence",
                                            "sinewave",
                                            "squarewave",
                                            "heavisine",
                                            "bumps",
                                            "blocks",
                                            "doppler",
                                            "ramp",
                                            "cusp",
                                            "sing",
                                            "hisine",
                                            "losine",
                                            "linchirp",
                                            "twochirp",
                                            "quadchirp",
                                            "mishmash",
                                            "wernersorrows",
                                            "leopold"
                                            ,"astsasoi"
                                            ,"astsastar"
                                            ,"astsacmort"
                                            ,"astsanyse"
                                            ,"sunspotmonthly"
                                            ,"arimapdq"
                                            ,"custom"
                                            )
          ) {
        # Fs <- 16000
        # nSecsDuration <- 4
        # nBits <- 16
        # stdv <- 0.196 # 10^(-1/sqrt(2)) # 10^(-7/10) # 0.2
        # y <- handles$generatorWave # (2^(nBits-1)-1) * rnorm(nSecsDuration*Fs, mean=0, sd=stdv)
        objWav <- handles$generatorWave
        if (!isS4(objWav)) {return()}
        y <- objWav@left # assumed monophonic, data stored within left-channel only
        #if (objWav@stereo) {xright <- objWav@right} # if stereo-recording
        Fs <- objWav@samp.rate
        # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
        # isPCM <- objWav@pcm
        nBits <- objWav@bit
        # FundFreq <- tuneR::FF(tuneR::periodogram(objWav))
      # } else if (input$audiogeneratorsignal=='custom') {
      #   # TBD
      }
    }
                               
    if (!exists("y")) {return()}
                               
    yfiltered <- try(signal::filter(filt=input$edit_gain*handlesb(), 
                                    a=handlesa(), 
                                    y
                                    ),silent=TRUE)
    # print(yfiltered)
    
    objWave <- try(tuneR::Wave(left=matrix(data=round(32767 * yfiltered/max(abs(yfiltered),na.rm=TRUE)),
                                          ncol=1), # nrow=length(yfiltered)),
                           samp.rate=Fs,
                           bit=16,
                           pcm=TRUE
                           ),silent=TRUE)
    # tuneR::writeWave(objWave, paste0(input$audiogeneratorsignal,"filtered.wav"))

    tdir <- "www"
    tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$audiogeneratorsignal),"filtered.wav"))
    # cat(file=stderr(),"input$audiogeneratorsignal:",input$audiogeneratorsignal,".\n")
    if (!file.exists(tfile) 
        # || (input$audiogeneratorsignal=="custom")
        ){
      try(tuneR::writeWave(objWave, tfile),silent=TRUE)
    }
    
    output$HTML5audioWidget2 <- renderUI({
      # if (is.null(input$filenameAudio$name) || (!nzchar(input$filenameAudio$name, keepNA = FALSE))) {return()}
      # if ((tools::file_ext(input$filenameAudio$name) == "wav") # || (tools::file_ext(input$filenameAudio$name) == "mp3")
      # ) {
      # https://groups.google.com/forum/#!topic/shiny-discuss/zO8hEFCxa0c; Andrew Caines, 19/09/2014
      if (input$inputsignalsource=="file") {
        p("Filtered: ",
        tags$audio(src = paste0(tools::file_path_sans_ext(input$filenameAudio$name),"filtered.wav"), type = "audio/wav", controls = TRUE, preload="metadata")
        )
      } else if (input$inputsignalsource=="generator") {
        tags$audio(src = paste0(input$audiogeneratorsignal,"filtered.wav"), type = "audio/wav", controls = TRUE, preload="metadata")
      } 
      
        # HTML(paste0('Filtered: <audio controls="controls" preload="metadata"> ',
        #             '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/wav"> ', # input$filenameAudio$datapath),' type="audio/wav"> ',
        #             # '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/wave"> ',
        #             # '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/ogg"> ',
        #             # '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/mpeg"> ',
        #             'Please Note: Your browser does not support HTML5 digital-audio playback. ',
        #             'Please upgrade to the latest version of your browser or operating system. ',
        #             '</audio>'
        #     )
        # )
      # }
    })

    
    # yfiltered <- yfiltered / max(yfiltered) # normalization
    # plot(t(yfiltered))
    # cat(file=stderr(),"L3462 head(yfiltered), before normalization:",head(yfiltered),".\n")
    # yfiltered <- tuneR::normalize(as.vector(yfiltered), unit=1) # yfiltered/max(yfiltered) # 
    
    # Wobj <- tuneR::Wave(left=matrix(data=round(32767 * yfiltered/max(abs(yfiltered)))), samp.rate = Fs, bit = 16, pcm = TRUE)
    # # print(Wobj)
    # tdir <- tempdir()
    # tdir <- "www"
    # tfile <- file.path(tdir, paste0("filtered",input$filenameAudio$name))
    # # tfile <- paste0("filtered",input$filenameAudio$name)
    # handles$tempfilteredfilelocation <- tfile
    
    # if (!file.exists(tfile) || input$audiogeneratorsignal=="custom") 
    #    tuneR::writeWave(Wobj, filename = tfile) # paste0("filtered",input$filenameAudio$name)) # 
    # # close(file.path(tdir, paste0("filtered",input$filenameAudio$name)))
    # Sys.sleep(5) # wait 5 seconds
    # 
    # # print(list.files(tdir, pattern = "\\.wav$"))
    # newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)
    # print(newWobjList)
   
    # cat(file=stderr(),"head(yfiltered):",head(yfiltered),".\n")
    # cat(file=stderr(),"max(yfiltered):",max(yfiltered),".\n")
    # cat(file=stderr(),"min(yfiltered):",min(yfiltered),".\n")
    # cat(file=stderr(),"Fs:",Fs,".\n")
    
    yfiltered <- yfiltered/max(abs(yfiltered),na.rm=TRUE) * 1.0 #  tuneR::normalize(as.vector(yfiltered),unit=1) # 
    if ("windows" %in% get_os()) {
      showNotification(
        ui = paste0("Now playing the filtered y signal, '",input$filenameAudio$name,"' (",Fs," Samples/s) ..."),
        duration = NULL, # 3,
        id="nowplayingfiltered",
        closeButton = TRUE,
        type = "message"
      )
      require(audio); try(audio::wait(audio::play(yfiltered, rate=Fs)),silent=TRUE)
      removeNotification(id="nowplayingfiltered",session)
    }
    # require(audio); try(audio::play(yfiltered, rate=Fs),silent=TRUE)
  })
  
    # output$HTML5audioWidget2 <- renderUI({
    #   # if (is.null(input$filenameAudio$name) || (!nzchar(input$filenameAudio$name, keepNA = FALSE))) {return()}
    #   # if ((tools::file_ext(input$filenameAudio$name) == "wav") # || (tools::file_ext(input$filenameAudio$name) == "mp3")
    #   # ) {
    #   p("Filtered: ",
    #   tags$audio(src = handles$tempfilteredfilelocation, type = "audio/wav", controls = TRUE, preload="metadata")
    #   )
    #     # HTML(paste0('Filtered: <audio controls="controls" preload="metadata"> ',
    #     #             '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/wav"> ', # input$filenameAudio$datapath),' type="audio/wav"> ',
    #     #             # '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/wave"> ',
    #     #             # '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/ogg"> ',
    #     #             # '<source src="',paste0("filtered",input$filenameAudio$name),'" type="audio/mpeg"> ',
    #     #             'Please Note: Your browser does not support HTML5 digital-audio playback. ',
    #     #             'Please upgrade to the latest version of your browser or operating system. ',
    #     #             '</audio>'
    #     #     )
    #     # )
    #   # }
    # })

  observeEvent(input$pb_runintro, {
    updateNavbarPage(session, 
                     inputId = "mainNavBarPage", 
                     selected = "Plots")

    # intro <- data.frame(element="#pb_ma",
    #                     intro="In Codd we trust")
    introjs(session, # options = list(steps= intro))
            options = list("nextLabel"="Next",
                           "prevLabel"="Prev",
                           "skipLabel"="Exit guided-tour"),
            events = list("oncomplete"='alert("Hope you enjoyed the tour ...")')
          )
  })
  
   # output$signaltimeplots ----
  output$signaltimeplots <- renderPlot(width = "auto", height = "auto", {
    if (input$inputsignalsource=="file") { 
    if (is.null(input$filenameAudio$name) || (!nzchar(input$filenameAudio$name, keepNA = FALSE))) {return()}
    if (tools::file_ext(input$filenameAudio$name) == "wav") {
      objWav <- try(tuneR::readWave(input$filenameAudio$name),silent=TRUE)
      if (isS4(objWav)) {
      y <- objWav@left # assumed monophonic, data stored within left-channel only
      #if (objWav@stereo) {xright <- objWav@right} # if stereo-recording
      Fs <- objWav@samp.rate
      # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
      
      nBits <- objWav@bit
      # SIZEwav <- length(y)
      #if (interactive()) tuneR::play(objWav)
      # Sys.sleep(1)
      # FundFreq <- tuneR::FF(tuneR::periodogram(objWav))
      }
    } else if (tools::file_ext(input$filenameAudio$name) == "mp3") {
      objMP3 <- try(tuneR::readMP3(input$filenameAudio$name),silent=TRUE)
      if (isS4(objMP3)) {
      y <- objMP3@left # assumed monophonic, data stored within left-channel only
      #if (objMP3@stereo) {xright <- objMP3@right} # if stereo-recording
      Fs <- objMP3@samp.rate
      # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
      nBits <- objMP3@bit
      # SIZEwav <- length(y)
      #if (interactive()) tuneR::play(objMP3)
      # Sys.sleep(1)
      
      Wobj <- tuneR::Wave(left=matrix(data=round(32767 * y/max(abs(y),na.rm=TRUE)),
                                         ncol=1), 
                          samp.rate = as.numeric(Fs), 
                          bit = 16, 
                          pcm = TRUE)
      # print(Wobj)
      # tdir <- tempdir()
      tdir <- "www"
      tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav"))
      # tfile <- paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")
      # tfile <- input$filenameAudio$name
      cat(file=stderr(),"L4369 tfile:",tfile,".\n")
      handles$tempfilteredfilelocation <- tfile
      
      if (!file.exists(tfile) 
          # || (input$audiogeneratorsignal=="custom")
          ) {
        try(tuneR::writeWave(Wobj, filename = tfile),silent=TRUE) # input$filenameAudio$name) # 
      }
      # close(file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")))
      Sys.sleep(5) # wait 5 seconds
  
      # print(list.files(tdir, pattern = "\\.wav$"))
      newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)

      }
    } else if (tools::file_ext(input$filenameAudio$name) == "mat") {
      matmusic <- try(R.matlab::readMat(input$filenameAudio$name),silent=TRUE) # y= audio-signal, and Fs=8192 samples-per-second
      if (!is.list(matmusic)) return()
      y <- matmusic$y
      print(str(y))
      Fs <- as.numeric(matmusic$Fs)
      nBits <- 16 # assumed!?!?

      # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
      
      Wobj <- tuneR::Wave(left=matrix(data=round(32767 * y/max(abs(y),na.rm=TRUE)),
                                         ncol=1), 
                          samp.rate = as.numeric(Fs), 
                          bit = 16, 
                          pcm = TRUE)
      # print(Wobj)
      # tdir <- tempdir()
      tdir <- "www"
      tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav"))
      # tfile <- paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")
      # tfile <- input$filenameAudio$name
      cat(file=stderr(),"L4395 tfile:",tfile,".\n")
      handles$tempfilteredfilelocation <- tfile
      
      if (!file.exists(tfile) 
          # || (input$audiogeneratorsignal=="custom")
          ) {
        try(tuneR::writeWave(Wobj, filename = tfile),silent=TRUE) # input$filenameAudio$name) # 
      }
      # close(file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")))
      Sys.sleep(5) # wait 5 seconds
  
      # print(list.files(tdir, pattern = "\\.wav$"))
      newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)

    } else {return()}
    } else { # using generator
            if (input$audiogeneratorsignal %in% c("whitenoise",
                                            "pinknoise",
                                            "pulsed",
                                            "sawtooth",
                                            # "silence",
                                            "sinewave",
                                            "squarewave",
                                            "heavisine",
                                            "bumps",
                                            "blocks",
                                            "doppler",
                                            "ramp",
                                            "cusp",
                                            "sing",
                                            "hisine",
                                            "losine",
                                            "linchirp",
                                            "twochirp",
                                            "quadchirp",
                                            "mishmash",
                                            "wernersorrows",
                                            "leopold"
                                            ,"astsasoi"
                                            ,"astsastar"
                                            ,"astsacmort"
                                            ,"astsanyse"
                                            ,"sunspotmonthly"
                                            ,"arimapdq"
                                            ,"custom"
                                            )
          ) {

        # Fs <- 16000
        # nSecsDuration <- 4
        # nBits <- 16
        # stdv <- 0.196 # 10^(-1/sqrt(2)) # 10^(-7/10) # 0.2
        # y <- handles$generatorWave # (2^(nBits-1)-1) * rnorm(nSecsDuration*Fs, mean=0, sd=stdv)
        objWav <- handles$generatorWave
        if (!isS4(objWav)) return()
        y <- objWav@left # assumed monophonic, data stored within left-channel only
        #if (objWav@stereo) {xright <- objWav@right} # if stereo-recording
        Fs <- objWav@samp.rate
        # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
        # isPCM <- objWav@pcm
        nBits <- objWav@bit
        # FundFreq <- tuneR::FF(tuneR::periodogram(objWav))
      # } else if (input$audiogeneratorsignal=='custom') {
      #   # TBD
      }
    }
    
    # print(y)
    yfiltered <- try(signal::filter(filt=input$edit_gain*handlesb(), 
                                    a=handlesa(), 
                                    y
                                    ),silent=TRUE)
    # yfiltered <- yfiltered / max(yfiltered,na.rm=TRUE) # normalization
    # plot(tyfiltered)
    # print(head(yfiltered))
    # yfiltered <- tuneR::normalize(tuneR::Wave(left=as.vector(yfiltered,mode="numeric"))) # yfiltered/max(yfiltered,na.rm=TRUE) # 

    par(mfrow = c(2, 2))
    # par(mgp = c(2.5, 1, 0)) # line for axis-title, axis-labels and axis-line
    par(mar=c(4.5, 2.5, 3.5, 2)) # c(bottom, left, top, right)
    
    # y <- y/max(abs(y),na.rm=TRUE) * 1.0 #  tuneR::normalize(as.vector(y),unit=1) # 
    plot(
      (0:(length(y)-1))/Fs, # length(y)/Fs*(0:length(y))/Fs,
      y,
      type="l",
      col="blue",
      main='Time-Domain Plot -- Original Signal',
      xlab='Time (s) (shifted to begin at zero)',
      ylab=""
    )
    grid(); abline(h=0); abline(v=0)
    
    # plot(0, 0, type='n')
    
    par(mfg=c(2,1))

    plot(
      (0:(length(yfiltered)-1))/Fs, # length(y)/Fs*(0:length(y))/Fs,
      yfiltered, # /45, # huh!??!? -- perhaps, must normalize the amplitudes somehow?
      type="l",
      col="red",
      main='Time-Domain Plot -- Filtered Signal',
      xlab='Time (s) (shifted to begin at zero)',
      ylab=""
    )
    grid(); abline(h=0); abline(v=0)

    par(mfrow = c(1, 1))
    par(mgp = c(3, 1, 0))
    par(mar=c(5, 4, 4, 2))
  })
    
   # output$specgrams ----
  output$specgrams <- renderPlot(width = "auto", height = "auto", {
    req(input$stretchyslider3range)
    par(mfrow = c(2, 2))
    # par(mgp = c(2.5, 1, 0)) # line for axis-title, axis-labels and axis-line
    # par(mar=c(1, 1, 1, 1)) # c(bottom, left, top, right)
    
    # require(tuneR)
    #tuneR::setWavPlayer('"C:/Program Files/Windows Media Player/wmplayer.exe"')
    #[x,Fs,nBits]= wavread("test");
    # print(input$filenameAudio$name)
    if (input$inputsignalsource=="file") { 
    if (is.null(input$filenameAudio$name) || (!nzchar(input$filenameAudio$name, keepNA = FALSE))) {return()}
    if (tools::file_ext(input$filenameAudio$name) == "wav") {
      objWav <- try(tuneR::readWave(input$filenameAudio$name),silent=TRUE)
      if (isS4(objWav)) {
      y <- objWav@left # assumed monophonic, data stored within left-channel only
      #if (objWav@stereo) {xright <- objWav@right} # if stereo-recording
      Fs <- objWav@samp.rate
      # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
      
      nBits <- objWav@bit
      # SIZEwav <- length(y)
      #if (interactive()) tuneR::play(objWav)
      # Sys.sleep(1)
      # FundFreq <- tuneR::FF(tuneR::periodogram(objWav))
      }
    } else if (tools::file_ext(input$filenameAudio$name) == "mp3") {
      objMP3 <- try(tuneR::readMP3(input$filenameAudio$name),silent=TRUE)
      if (isS4(objMP3)) {
      y <- objMP3@left # assumed monophonic, data stored within left-channel only
      #if (objMP3@stereo) {xright <- objMP3@right} # if stereo-recording
      Fs <- objMP3@samp.rate
      # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
      nBits <- objMP3@bit
      # SIZEwav <- length(y)
      #if (interactive()) tuneR::play(objMP3)
      # Sys.sleep(1)
      
      Wobj <- tuneR::Wave(left=matrix(data=round(32767 * y/max(abs(y),na.rm=TRUE)),
                                         ncol=1), 
                          samp.rate = as.numeric(Fs), 
                          bit = 16, 
                          pcm = TRUE)
      # print(Wobj)
      # tdir <- tempdir()
      tdir <- "www"
      tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav"))
      # tfile <- paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")
      # tfile <- input$filenameAudio$name
      cat(file=stderr(),"L4531 tfile:",tfile,".\n")
      handles$tempfilteredfilelocation <- tfile
      
      if (!file.exists(tfile) 
          # || (input$audiogeneratorsignal=="custom")
          ) {
        try(tuneR::writeWave(Wobj, filename = tfile),silent=TRUE) # input$filenameAudio$name) # 
      }
      # close(file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")))
      Sys.sleep(5) # wait 5 seconds
  
      # print(list.files(tdir, pattern = "\\.wav$"))
      newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)

      }
    } else if (tools::file_ext(input$filenameAudio$name) == "mat") {
      matmusic <- try(R.matlab::readMat(input$filenameAudio$name),silent=TRUE) # y= audio-signal, and Fs=8192 samples-per-second
      if (!is.list(matmusic)) return()
      y <- matmusic$y
      print(str(y))
      Fs <- as.numeric(matmusic$Fs)
      nBits <- 16 # assumed!?!?

      # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
      
      Wobj <- tuneR::Wave(left=matrix(data=round(32767 * y/max(abs(y),na.rm=TRUE)),
                                         ncol=1), 
                          samp.rate = as.numeric(Fs), 
                          bit = 16, 
                          pcm = TRUE)
      # print(Wobj)
      # tdir <- tempdir()
      tdir <- "www"
      tfile <- file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav"))
      # tfile <- paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")
      # tfile <- input$filenameAudio$name
      cat(file=stderr(),"L4557 tfile:",tfile,".\n")
      handles$tempfilteredfilelocation <- tfile
      
      if (!file.exists(tfile) 
          # || (input$audiogeneratorsignal=="custom")
          ) {
        try(tuneR::writeWave(Wobj, filename = tfile),silent=TRUE) # input$filenameAudio$name) # 
      }
      # close(file.path(tdir, paste0(tools::file_path_sans_ext(input$filenameAudio$name),".wav")))
      Sys.sleep(5) # wait 5 seconds
  
      # print(list.files(tdir, pattern = "\\.wav$"))
      newWobjList <- try(tuneR::readWave(tfile,header = TRUE),silent=TRUE)

    } else {return()}
    } else if (input$inputsignalsource=="generator") { # using generator
      if (input$audiogeneratorsignal %in% c("whitenoise",
                                            "pinknoise",
                                            "pulsed",
                                            "sawtooth",
                                            "silence",
                                            "sinewave",
                                            "squarewave",
                                            "heavisine",
                                            "bumps",
                                            "blocks",
                                            "doppler",
                                            "ramp",
                                            "cusp",
                                            "sing",
                                            "hisine",
                                            "losine",
                                            "linchirp",
                                            "twochirp",
                                            "quadchirp",
                                            "mishmash",
                                            "wernersorrows",
                                            "leopold"
                                            ,"astsasoi"
                                            ,"astsastar"
                                            ,"astsacmort"
                                            ,"astsanyse"
                                            ,"sunspotmonthly"
                                            ,"arimapdq"
                                            ,"custom"
                                            )
          ) {

        # Fs <- 16000
        # nSecsDuration <- 4
        # nBits <- 16
        # stdv <- 0.196 # 10^(-1/sqrt(2)) # 10^(-7/10) # 0.2
        # y <- handles$generatorWave # (2^(nBits-1)-1) * rnorm(nSecsDuration*Fs, mean=0, sd=stdv)
        objWav <- handles$generatorWave
        if (!isS4(objWav)) return()
        y <- objWav@left # assumed monophonic, data stored within left-channel only
        #if (objWav@stereo) {xright <- objWav@right} # if stereo-recording
        Fs <- objWav@samp.rate
        # updateNumericInput(session, inputId = "samplingfreq", value = Fs)
        # isPCM <- objWav@pcm
        nBits <- objWav@bit
        # FundFreq <- tuneR::FF(tuneR::periodogram(objWav))
        # }
      # } else if (input$audiogeneratorsignal=='custom') {
      #   # TBD
      }
    }
    
    # print(y)
    yfiltered <- try(signal::filter(filt=input$edit_gain*handlesb(), 
                                a=handlesa(), 
                                y
                                ),silent=TRUE)
    # yfiltered <- yfiltered / max(yfiltered,na.rm=TRUE) # normalization
    # plot(tyfiltered)
    # print(head(yfiltered))
    # yfiltered <- tuneR::normalize(tuneR::Wave(left=as.vector(yfiltered,mode="numeric"))) # yfiltered/max(yfiltered,na.rm=TRUE) # 

    # y <- y/max(abs(y),na.rm=TRUE) * 1.0 #  tuneR::normalize(as.vector(y),unit=1) #   
    
    if (!exists("Fs")) {return()}
    
minSpgFreq <- trunc(input$stretchyslider3range[1] * (Fs/2)) # /(input$samplingfreq/2) # max(c(0.25* 44100/2, 0.01* Fs/2 ) ,na.rm=TRUE)  # Hz - min-frequency to display
maxSpgFreq <- trunc(input$stretchyslider3range[2] * (Fs/2)) # /(input$samplingfreq/2) # min(c(Fs/2, 0.80* 44100/2 ) ) # Hz - max-frequency to display

# print(minSpgFreq)
# print(maxSpgFreq)

step <- max(c(trunc(0.005*Fs),2),na.rm=TRUE)
#windowW <- trunc(40*Fs/1000)          # 40 ms data window
windowW <- max(c(trunc(0.040*Fs),2),na.rm=TRUE)
fftn <- 2^ceiling(log2(abs(windowW))) # next highest power of 2
#fftn <- trunc(0.020*Fs)
#spg= specgram(wav$sound, fftn, Fs, windowW, windowW-step)
#spg <- signal::specgram(x, n=fftn, Fs=Fs, window=windowW, overlap=0.010*Fs)
spg <- signal::specgram(y, n=fftn, Fs=Fs, window=windowW, overlap=windowW-step)
#spg <- signal::specgram(x)
#S <- abs(spg$S[2:(fftn*4000/Fs),])   # magnitude in range 0<f<=4000 Hz.
S <- Mod(spg$S[(max(c(2,fftn*minSpgFreq/Fs),na.rm=TRUE)):(fftn*maxSpgFreq/Fs),])
maxS1 <- max(abs(S),na.rm=TRUE) # store this, for later usage on the second-plot below
#S <- S/max(S,na.rm=TRUE)         # normalize magnitude so that max is 0 dB.
S <- S/maxS1 # normalize
#S[S < 10^(-40/10)] <- 10^(-40/10)    # clip below -40 dB.
#S[S > 10^(-3/10)] <- 10^(-3/10)      # clip above -3 dB.
#image(x=list(x=0:1,y=0:4000,z=t(20*log10(S))), col=heat.colors(12))  #, col= gray(0:255 / 255))
#image(t(20*log10(S)), col=heat.colors(12),axes=FALSE)
dBdata <- t(20*log10(S))
image(dBdata, col=matlab::jet.colors(12),axes=FALSE)
if (input$includeContoursInSpectrograms) {
  contour(dBdata, 
        # levels = seq(90, 200, by = 5), 
        add = TRUE
        ,col = "peru"
        ,labcex = 0.6, drawlabels = FALSE
        )
}
axis(1,
     at=seq(0, length(y), by=windowW)/Fs,
     labels=round(length(y)/Fs*seq(0, length(y), by=windowW)/Fs,2)
     )
axis(2,
     at=seq(0, 1, by=1/5) * input$samplingfreq/2,
     labels=round(seq(minSpgFreq,maxSpgFreq, by=(maxSpgFreq-minSpgFreq)/5) / (Fs/2), 2) # * input$samplingfreq,2)
     )
#grid(); abline(h=0); abline(v=0)
abline(h=(input$slider1-minSpgFreq/ (Fs/2)) / ((maxSpgFreq-minSpgFreq)/ (Fs/2)), # / (Fs/2) * input$samplingfreq, #/(maxSpgFreq/if (input$normalizedMagPlotAmplitude) {Fs/2}else{1}),
       col = "black", lty="dashed")
#axis xy
#view(90,90)
title('Spectrogram -- Original Signal',
  xlab='Time (s)',
  ylab=paste0('Freq',if (input$freqaxisunits == "zero2one") {" (normalized)"} else {" (Hz)"})
#  ylab=expression(paste('Norm. Freq. ', omega, '; i.e. from ', 0, ' to ', pi, ' rads/sec', phantom(.)==(F[s]/2))),
  )
# print(FundFreq)
# try(points(x=par("usr")[2], y=FundFreq*1000/(Fs/2), pch=23, col="red"),silent=TRUE)

if (input$secondaryaxis) {
  axis(4,
     at=seq(0, 1, by=1/5),
     labels=round(seq(minSpgFreq,maxSpgFreq, by=(maxSpgFreq-minSpgFreq)/5), 0) # * input$samplingfreq,2)
  )
}

# print(Fs)
# peaksmtx <- seewave::localpeaks(seewave::spec(y,f=Fs,plot=FALSE),bands=10,plot=FALSE,labels=TRUE)
# print(seewave::meanspec(y,f=Fs,plot=FALSE))
# print(peaksmtx)
# text(x=20*log10(peaksmtx[,2]),y=peaksmtx[,1],pos=4,cex=0.7)

dBFFT <- try(20*log10(Mod(fft(y))),silent=TRUE)
if (is.null(dBFFT)) return()
# print(head(dBFFT))
# print(length(dBFFT))
# print(min(dBFFT))
# print(max(dBFFT),na.rm=TRUE)

dBFFT[dBFFT>320] <- 320
dBFFT[dBFFT<0] <- 0
# print(head(dBFFT))

# print(minSpgFreq)
# print(maxSpgFreq)
# print(Fs/2)
# print(minSpgFreq:maxSpgFreq)
# print((minSpgFreq:maxSpgFreq) * (length(dBFFT)/2 * (Fs/2)))
# print(head(dBFFT[(minSpgFreq:maxSpgFreq) * (length(dBFFT)/2 * (Fs/2))]))
maxdBFFT <- max(dBFFT[(max(c(1,minSpgFreq)):maxSpgFreq) * (length(dBFFT)/2 / (Fs/2))],na.rm=TRUE) # (minSpgFreq:maxSpgFreq)]) #  * (length(dBFFT)/2 * (Fs/2))]) # [1:(length(dBFFT)/2* maxSpgFreq / (Fs/2))])
# print(maxdBFFT)

# print(dBFFT[(max(c(1,minSpgFreq),na.rm=TRUE):maxSpgFreq) * (length(dBFFT)/2 / (Fs/2))])

plot(
  -maxdBFFT+dBFFT[(max(c(1,minSpgFreq),na.rm=TRUE):maxSpgFreq) * (length(dBFFT)/2 / (Fs/2))],
  ((max(c(1,minSpgFreq),na.rm=TRUE):maxSpgFreq) * (length(dBFFT)/2 / (Fs/2))) / (length(dBFFT)/2 ),# / (Fs/2),
  # x=dBFFT[1:maxSpgFreq], 
  # y=(1:maxSpgFreq) / (Fs/2), #  * input$samplingfreq, # /if (input$freqaxisunits == "zero2one") {Fs/2}else{1}, 
     type="o", 
     col="blue", 
     xaxs="i",yaxs="i", 
     # yaxt="n",
     xlim=c(-25,0), # -maxdBFFT+c(5*floor(maxdBFFT/5)-20,5*ceiling(maxdBFFT/5)),
  xlab='Levels (dB)',
  ylab=paste0('Freq',if (input$freqaxisunits == "zero2one") {" (normalized)"} else {" (Hz)"})
, main='Spectrum -- original-signal'
     )
# try(points(x=par("usr")[2], y=FundFreq*1000/(Fs/2), pch=23, col="red"),silent=TRUE)
# pracma::errorbar(
#   -maxdBFFT+0.5*dBFFT[(max(c(1,minSpgFreq),na.rm=TRUE):maxSpgFreq) * (length(dBFFT)/2 / (Fs/2))],
#   ((max(c(1,minSpgFreq),na.rm=TRUE):maxSpgFreq) * (length(dBFFT)/2 / (Fs/2))) / (length(dBFFT)/2 ),
#   xerr=-maxdBFFT+dBFFT[(max(c(1,minSpgFreq),na.rm=TRUE):maxSpgFreq) * (length(dBFFT)/2 / (Fs/2))],
#   bar.col="cyan",
#   with=FALSE,
#   add=TRUE
# )
#plot(abs(fft(x))[1:(maxSpgFreq)], 1:(maxSpgFreq), type="l", col="blue", xaxs="i",yaxs="i", yaxt="n")
#points(20*log10(abs(fft(x)))[f],f)
# axis(2, at=seq(0,1, by=1/5) * maxSpgFreq / (Fs/2) ) # * input$samplingfreq) # /if (input$normalizedMagPlotAmplitude) {Fs/2}else{1})
grid(); abline(h=0); abline(v=0)
# abline(h=(Fstop+Fpass)/2, col="red", lty="dashed")
abline(h=input$slider1, #  / (Fs/2) * input$samplingfreq, # /(maxSpgFreq/if (input$normalizedMagPlotAmplitude) {Fs/2}else{1}),
       col = "magenta", lty="dashed")


step <- max(c(trunc(0.005*Fs),2),na.rm=TRUE)
#windowW <- trunc(40*Fs/1000)          # 40 ms data window
windowW <- max(c(trunc(0.040*Fs),2),na.rm=TRUE)
fftn <- 2^ceiling(log2(abs(windowW))) # next highest power of 2
#fftn <- trunc(0.020*Fs)
#spg= specgram(wav$sound, fftn, Fs, windowW, windowW-step)
#spg <- signal::specgram(x, n=fftn, Fs=Fs, window=windowW, overlap=0.010*Fs)
spg <- signal::specgram(yfiltered, n=fftn, Fs=Fs, window=windowW, overlap=windowW-step)
#spg <- signal::specgram(x)
#S <- abs(spg$S[2:(fftn*4000/Fs),])   # magnitude in range 0<f<=4000 Hz.
S <- Mod(spg$S[(max(c(2,fftn*minSpgFreq/Fs),na.rm=TRUE)):(fftn*maxSpgFreq/Fs),])
S <- S/max(S,na.rm=TRUE)         # normalize magnitude so that max is 0 dB.
# S <- S/maxS1 #* 1.1 # / (10^(80/10)) # normalize by same amount as first plot, minus some corrective-amount (e.g. -80dB?), in order to approximately match colours
#S[S < 10^(-40/10)] <- 10^(-40/10)    # clip below -40 dB.
#S[S > 10^(-3/10)] <- 10^(-3/10)      # clip above -3 dB.
#image(x=list(x=0:1,y=0:4000,z=t(20*log10(S))), col=heat.colors(12))  #, col= gray(0:255 / 255))
#image(t(20*log10(S)), col=heat.colors(12),axes=FALSE)
dBdata <- t(20*log10(S))
image(dBdata, col=matlab::jet.colors(12),axes=FALSE)
if (input$includeContoursInSpectrograms) {
  contour(dBdata, 
        # levels = seq(90, 200, by = 5), 
        add = TRUE
        ,col = "peru"
        ,labcex = 0.6, drawlabels = FALSE
        )
}
axis(1,
     at=seq(0, length(y), by=windowW)/Fs,
     labels=round(length(y)/Fs*seq(0, length(y), by=windowW)/Fs,2)
     )
axis(2,
     at=seq(0, 1, by=1/5),
     labels=round(seq(minSpgFreq,maxSpgFreq, by=(maxSpgFreq-minSpgFreq)/5) / (Fs/2), 2) # / (input$samplingfreq),2) # /if (input$normalizedMagPlotAmplitude) {Fs/2}else{1},2)
     )
#grid(); abline(h=0); abline(v=0)
abline(h=(input$slider1-minSpgFreq/ (Fs/2)) / ((maxSpgFreq-minSpgFreq)/ (Fs/2)), # * input$samplingfreq, # /(maxSpgFreq/if (input$normalizedMagPlotAmplitude) {Fs/2}else{1}),
       col = "black", lty="dashed")
#axis xy
#view(90,90)
title('Spectrogram -- Filtered Signal',
  xlab='Time (s)',
  ylab=paste0('Freq',if (input$freqaxisunits == "zero2one") {" (normalized)"} else {" (Hz)"})
#  ylab=expression(paste('Norm. Freq. ', omega, '; i.e. from ', 0, ' to ', pi, ' rads/sec', phantom(.)==(F[s]/2))),
)
# try(points(x=par("usr")[2], y=FundFreq*1000/(Fs/2), pch=23, col="red"),silent=TRUE)

if (input$secondaryaxis) {
  axis(4,
     at=seq(0, 1, by=1/5),
     labels=round(seq(minSpgFreq,maxSpgFreq, by=(maxSpgFreq-minSpgFreq)/5), 0) # * input$samplingfreq,2)
  )
}

# mtext(
#   paste0(
#     fileName,
#     ", Fst, Fp, Ap, As, Fstop, Fpass= ",
#     list(c(Fst, Fp, Ap, As, Fstop, Fpass))
#   ),
#   side=1, line=-1, outer=TRUE, cex=0.6
# )

    # print(signal::specgram(y,1024,Fs)) # ,882,772))
    # print(signal::specgram(yfiltered,1024,Fs)) # ,882,772))
    
# require(seewave)
# seewave::spectro( y, f=Fs, osc=TRUE, listen=FALSE )
# seewave::spectro( yfiltered, f=Fs, osc=TRUE, listen=FALSE )
# mtext(
#   'seewave::spectro( y, f=Fs, osc=TRUE, listen=FALSE )',
#   side=1, line=-1, outer=TRUE, cex=0.6
# )

#subplot
dBFFTfiltered <- try(20*log10(Mod(fft(t(yfiltered)))),silent=TRUE)
if (is.null(dBFFTfiltered)) return()

dBFFTfiltered[dBFFTfiltered>320] <- 320
dBFFTfiltered[dBFFTfiltered<0] <- 0

maxdBFFTfiltered <- max(dBFFTfiltered[(length(dBFFTfiltered)/2* minSpgFreq / (Fs/2)):(length(dBFFTfiltered)/2* maxSpgFreq / (Fs/2))],na.rm=TRUE) # [1:(length(dBFFTfiltered)/2* maxSpgFreq / (Fs/2))])

plot(
  -maxdBFFT+dBFFTfiltered[(max(c(1,minSpgFreq),na.rm=TRUE):maxSpgFreq) * (length(dBFFTfiltered)/2 / (Fs/2))],
  ((max(c(1,minSpgFreq),na.rm=TRUE):maxSpgFreq) * (length(dBFFTfiltered)/2 / (Fs/2))) / (length(dBFFTfiltered)/2 ),# / (Fs/2),
  # -maxdBFFT+dBFFTfiltered[((max(c(1,minSpgFreq),na.rm=TRUE):maxSpgFreq) * (length(dBFFTfiltered)/2 / (Fs/2)))],
  # (((max(c(1,minSpgFreq),na.rm=TRUE):maxSpgFreq) * (length(dBFFTfiltered)/2 / (Fs/2)))) / (length(dBFFTfiltered)/2), # / (Fs/2),
  # x=dBFFTfiltered[1:maxSpgFreq],
  #    y=(1:maxSpgFreq) / (Fs/2), # * input$samplingfreq, # /if (input$normalizedMagPlotAmplitude) {Fs/2}else{1},
     type="o",
     col="blue",
     xaxs="i",yaxs="i",
     # yaxt="n",
     xlim=c(min(c(-25,
                  -maxdBFFT+5*floor(maxdBFFTfiltered/5)-25
                  ), na.rm = TRUE
                ),
            max(c(0, # -maxdBFFT+5*ceiling(maxdBFFT/5),
                  -maxdBFFT+maxdBFFTfiltered # 5*ceiling(maxdBFFTfiltered/5)
                  ), na.rm = TRUE
                )
            ), # c(-maxdBFFT+5*floor(maxdBFFT/5)-20, max(c(-maxdBFFT+5*ceiling(maxdBFFT/5),-maxdBFFTfiltered+5*ceiling(maxdBFFTfiltered/5)))),
  xlab='Levels (dB)',
  ylab=paste0('Freq',if (input$freqaxisunits == "zero2one") {" (normalized)"} else {" (Hz)"})
, main='Spectrum -- filtered-signal'
     )
# try(points(x=par("usr")[2], y=FundFreq*1000/(Fs/2), pch=23, col="red"),silent=TRUE)
# barplot(
#   # rbind(
#     -maxdBFFT+dBFFTfiltered[((max(c(1,minSpgFreq)):maxSpgFreq) * (length(dBFFTfiltered)/2 / (Fs/2)))],
#               # -25),
#         beside=FALSE,
#         horiz=TRUE,
#         col="blue", # c("white","blue"),
#         border=NA,
#         xlab='Levels (dB)',
#         ylab=paste0('Freq',if (input$freqaxisunits == "zero2one") {" (normalized)"} else {" (Hz)"}),
#         main='Spectrum -- filtered-signal',
#         # xlim=-maxdBFFT+c(10*floor(maxdBFFTfiltered/10)-25, 5*ceiling(maxdBFFT/5)),
#         bty="o",
#         xpd=FALSE
#         )
#plot(abs(fft(x))[1:(maxSpgFreq)], 1:(maxSpgFreq), type="l", col="blue", xaxs="i",yaxs="i", yaxt="n")
# axis(2, 
#      at=seq(minSpgFreq,maxSpgFreq, by=(maxSpgFreq-minSpgFreq)/5) / (Fs/2) # seq(0,1, by=1/5) * (maxSpgFreq-minSpgFreq) / (Fs/2),
#      # ,labels=round(seq(minSpgFreq,maxSpgFreq, by=(maxSpgFreq-minSpgFreq)/5) / (Fs/2), 2) # * input$samplingfreq) #  /if (input$normalizedMagPlotAmplitude) {Fs/2}else{1}
#      )
grid(); abline(h=0); abline(v=0)
# abline(h=(Fstop+Fpass)/2, col="red", lty="dashed")
abline(h=input$slider1, # (maxSpgFreq-minSpgFreq)*(input$slider1-minSpgFreq/ (Fs/2)) / ((maxSpgFreq-minSpgFreq)/ (Fs/2)), #, # / (Fs/2) * input$samplingfreq, # /(maxSpgFreq  /if (input$normalizedMagPlotAmplitude) {Fs/2}else{1}), 
                        col = "magenta", lty="dashed")
# abline(h=0.5*(Fs/2))
# http://r.789695.n4.nabble.com/Horizontal-grid-in-background-of-barplot-td4642081.html
# Sep/2012, 'Cable, Sam B Civ USAF AFMC AFRL/RVBXI'
# print(par("usr")) # c(x1, x2, y1, y2)
# print(input$slider1-minSpgFreq/ (Fs/2))
# print((input$slider1-minSpgFreq/ (Fs/2)) / ((maxSpgFreq-minSpgFreq)/ (Fs/2)))
# print((maxSpgFreq-minSpgFreq)*(input$slider1-minSpgFreq/ (Fs/2)) / ((maxSpgFreq-minSpgFreq)/ (Fs/2)))
# print(par("usr")[2])
# print(par("usr")[3])
# print(par("usr")[4])

    par(mfrow = c(1, 1))
    par(mgp = c(3, 1, 0))
    par(mar=c(5, 4, 4, 2))
  })

  #   output$slider3Widget <- renderUI({
  #   sliderInput(
  #     inputId = "slider3",
  #     label = "Frequency-Range",
  #     min = 0.0,
  #     max = 1.0 * input$samplingfreq / 2,
  #     value = c(0.01,0.80) * input$samplingfreq / 2,
  #     step = 0.01 * input$samplingfreq / 2,
  #     ticks = TRUE,
  #     animate = animationOptions(interval = 300, loop = FALSE),
  #     sep = " "
  #   )
  # })
    
      output$stretchyslider3rangeWidget <- renderUI({
    sliderInput(
      inputId = "stretchyslider3range",
      label = "Frequency-Range",
      min = handles$minslider3range,
      max = handles$maxslider3range,
      value = c(0.01, 0.80) * input$samplingfreq / 2,
      step = handles$inputstretchyslider3step, # 0.01,
      ticks = TRUE,
      animate = animationOptions(interval = 300, loop = FALSE),
      sep = " ",
      dragRange = TRUE,
      post="pi"
    )
  })

        observeEvent(eventExpr = input$stretchyslider3range,
               handlerExpr = {
                 if ((abs(input$stretchyslider3range[2] - handles$maxslider3range) <
                     0.04 * abs(handles$maxslider3range - handles$minslider3range)) && 
                     (handles$maxslider3range<1)
                     ) {
                   handles$maxslider3range <- min(c(
                     round((
                       handles$maxslider3range +
                         (handles$maxslider3range - 1e-06) * handles$growSliders
                     ) / handles$inputstretchyslider3step
                     ) *
                       handles$inputstretchyslider3step,
                     round((
                       handles$maxslider3range +
                         (handles$maxslider3range - handles$minslider3range) *
                         handles$growSliders
                     ) / handles$inputstretchyslider3step
                     ) * handles$inputstretchyslider3step
                   ))
                   if (handles$maxslider3range>1) {handles$maxslider3range <- 1}
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider3range",
                     value = input$stretchyslider3range,
                     max = handles$maxslider3range
                   )
                   # updateSliderInput(
                   #   session,
                   #   inputId = "slider3",
                   #   value = handles$slider3value,
                   #   max = input$stretchyslider3range[2]
                   # )
                 }
                 if ((abs(input$stretchyslider3range[1] - handles$minslider3range) <
                     0.04 * abs(handles$maxslider3range - handles$minslider3range)) && 
                     (handles$minslider3range>0)
                     ) {
                   handles$minslider3range <- round((
                     handles$minslider3range -
                       (handles$maxslider3range - handles$minslider3range) *
                       handles$growSliders
                   ) / handles$inputstretchyslider3step
                   ) * handles$inputstretchyslider3step
                   if (handles$minslider3range<0) {handles$minslider3range <- 0}
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider3range",
                     value = input$stretchyslider3range,
                     min = handles$minslider3range
                   )
                   # updateSliderInput(
                   #   session,
                   #   inputId = "slider3",
                   #   value = handles$slider3value,
                   #   min = input$stretchyslider3range[1]
                   # )
                 }
                 if (abs(input$stretchyslider3range[2] - input$stretchyslider3range[1]) <
                     0.1 * abs(handles$maxslider3range - handles$minslider3range)) {
                   if (mean(c(input$stretchyslider3range)) < mean(c(handles$maxslider3range,
                                                                     handles$minslider3range))# && (handles$maxslider3range<1)
                       ) {
                     handles$maxslider3range <- max(
                       c(
                         input$stretchyslider3range[2] +
                           3L * handles$inputstretchyslider3step,
                         round((
                           handles$maxslider3range -
                             (handles$maxslider3range - handles$minslider3range) *
                             handles$growSliders
                         ) / handles$inputstretchyslider3step
                         ) *
                           handles$inputstretchyslider3step
                       )
                     ,na.rm=TRUE)
                     if (handles$maxslider3range>1) {handles$maxslider3range <- 1}
                     updateSliderInput(
                       session,
                       inputId = "stretchyslider3range",
                       value = input$stretchyslider3range,
                       max = handles$maxslider3range
                     )
                     # updateSliderInput(
                     #   session,
                     #   inputId = "slider3",
                     #   value = handles$slider3value,
                     #   max = input$stretchyslider3range[2]
                     # )
                   }
                   else # if (handles$minslider3range>0) 
                     {
                     handles$minslider3range <- min(
                       c(
                         input$stretchyslider3range[1] -
                           3L * handles$inputstretchyslider3step,
                         round((
                           handles$minslider3range +
                             (handles$maxslider3range - handles$minslider3range) *
                             handles$growSliders
                         ) / handles$inputstretchyslider3step
                         ) *
                           handles$inputstretchyslider3step
                       )
                     )
                     if (handles$minslider3range<0) {handles$minslider3range <- 0}
                     updateSliderInput(
                       session,
                       inputId = "stretchyslider3range",
                       value = input$stretchyslider3range,
                       min = handles$minslider3range
                     )
                     # updateSliderInput(
                     #   session,
                     #   inputId = "slider3",
                     #   value = handles$slider3value,
                     #   min = input$stretchyslider3range[1]
                     # )
                   }
                 }
                 handles$slider3value <- input$slider3
               })
        
        output$filterFormWidget <- renderUI({
                      # conditionalPanel(
                      # condition = "input.filterForm=='df1'",
                  if (input$filterForm=='df1') {
                    tags$div(
                      title = "tooltip: IIR digital-filter, Direct-Form I\n(use browser's right-mouse-click/ context-menu for image download-options)",
                      img(src = "DirectFormI.png", align = "right", width = "100%"
                        , alt = "IIR digital-filter, Direct-Form I"
                      )
                    )
                    # )
                  } else if (input$filterForm=='df2') {
                    # conditionalPanel(
                    #   condition = "input.filterForm=='df2'",
                    tags$div(
                      title = "tooltip: IIR digital-filter, Direct-Form II\n(use browser's right-mouse-click/ context-menu for image download-options)",
                      img(src = "DirectFormII.png", align = "right", width = "100%"
                        , alt = "IIR digital-filter, Direct-Form II"
                      )
                    )
                    # )
                  }
        })

  # observeEvent (button) pb_mp ----
  observeEvent(eventExpr = input$pb_mp, handlerExpr = {
    updateSelectInput(session, inputId = "listbox_pole", choices = c(0L))
    handles$selectedpole <- NULL
    handles$selectedzero <- NULL
    handles$poleloc <- c(0L)
    if (!pracma::isempty(handles$connecttype)) {
      polestring <- which(handles$connecttype == "x")
      handles$connection[polestring,] <- 0L
      handles$connection[, polestring] <- 0L
      handles$connecttype[polestring] <- NA
    }
    updateTabsetPanel(session, inputId = "tabPoleZeroEditing", selected = "RealImag")
  })
  

  
  # observeEvent (button) pb_mz ----
  observeEvent(eventExpr = input$pb_mz, handlerExpr = {
    updateSelectInput(session, inputId = "listbox_zero", choices = c(0L))
    handles$selectedzero <- list(XData = NULL, YData = NULL)
    handles$zeroloc <- c(0L)
    if (!pracma::isempty(handles$connecttype)) {
      zerostring <- which(handles$connecttype == "o")
      handles$connection[zerostring,] <- 0L
      handles$connection[, zerostring] <- 0L
      handles$connecttype[zerostring] <- NA
    }
    updateTabsetPanel(session, inputId = "tabPoleZeroEditing", selected = "RealImag")
  })
  
  # observeEvent (button) pb_ma ----
  observeEvent(eventExpr = input$pb_ma, handlerExpr = {
    updateSelectInput(session, inputId = "listbox_pole", choices = c(0L))
    updateSelectInput(session, inputId = "listbox_zero", choices = c(0L))
    updateNumericInput(session, inputId = "edit_gain", value = 1L)
    handles$selectedpole <- list(XData = NULL, YData = NULL)
    handles$selectedzero <- list(XData = NULL, YData = NULL)
    handles$poleloc <- c(0L)
    handles$zeroloc <- c(0L)
    handles$connection <- NULL
    handles$connecttype <- NULL
    updateTabsetPanel(session, inputId = "tabPoleZeroEditing", selected = "RealImag")
  })
  observeEvent(eventExpr = input$ScaleToVerticalHeight,
               handlerExpr = {
                 scalePlotsToVerticalHeight <- input$ScaleToVerticalHeight
               })
  observeEvent(eventExpr = c(input$filecoordsImport),
               handlerExpr = {
                 shinyjs::disable(id = "pb_editpole")
                 shinyjs::disable(id = "pb_editzero")
                 shinyjs::disable(id = "pb_delpole")
                 shinyjs::disable(id = "pb_delzero")
               })
  observeEvent(eventExpr = input$normalizedMagPlotAmplitude,
               handlerExpr = {
                 if (input$normalizedMagPlotAmplitude == TRUE) {
                   filtb <- handlesb()
                   filta <- handlesa()
                   filtb[Re(filtb) > 1e+12] <- 1e+12
                   filtb[Re(filtb) < -1e+12] <- -1e+12
                   filta[Re(filta) > 1e+12] <- 1e+12
                   filta[Re(filta) < -1e+12] <- -1e+12
                   filtb[Im(filtb) > 1e+12] <- Re(filtb) + 1i * 1e+12
                   filtb[Im(filtb) < -1e+12] <- Re(filtb) - 1i * 1e+12
                   filta[Im(filta) > 1e+12] <- Re(filtb) + 1i * 1e+12
                   filta[Im(filta) < -1e+12] <- Re(filtb) - 1i * 1e+12
                   rv <- signal::freqz(
                     filtb,
                     filta,
                     region = "whole",
                     n = 2L ^ 20L,
                     Fs = 2 * pi * input$samplingfreq / 2
                   )
                   updateNumericInput(session, inputId = "edit_gain", value = 1/ max(Re(rv$h),na.rm=TRUE))
                 }
                 else {
                   updateNumericInput(session, inputId = "edit_gain", value = 1L)
                 }
               })
  
  # observeEvent (pulldown) customSignalinput  ----
  observeEvent(eventExpr = input$customsignalinput,
               handlerExpr = {
                 updateTextInput(session,
                                 inputId = "edit_customsignalText",
                                 value = input$customsignalinput)
               })
  
  # observeEvent (pulldown) commonFilters ----
  observeEvent(eventExpr = input$commonFilters,
               handlerExpr = {
                 shiny::req(input$commonFilters)
                 shinyjs::disable(id = "pb_editpole")
                 shinyjs::disable(id = "pb_editzero")
                 shinyjs::disable(id = "pb_delpole")
                 shinyjs::disable(id = "pb_delzero")
                 if (grepl("sample", input$commonFilters)) {
                   if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$commonFilters,ignore.case=FALSE))) return() # info-system security
                   chosenFilter <- try(eval(parse(text = input$commonFilters)),silent=TRUE)
                   updateSelectInput(session, inputId = "commonFilters", selected = chosenFilter)
                   return(NULL)
                 }
                 else {
                   chosenFilter <- input$commonFilters
                 }
                 if (grepl("Zpg|sftrans|bilinear", chosenFilter)) {
                   if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", chosenFilter,ignore.case=FALSE))) return() # info-system security
                   zpg <- try(eval(parse(text = chosenFilter)),silent=TRUE)
                   if (is.list(zpg)) {
                   handles$zeroloc <- zpg$zero
                   handles$poleloc <- zpg$pole
                   updateNumericInput(session, inputId = "edit_gain", value = zpg$gain)
                   b <- zpg$gain
                   length(b) <- 1L
                   }
                 }
                 else if (grepl(
                   "fir1|fir2|remez|spencerFilter|sgolay|Ma|chebwin|kaiser|bartlett|blackman|boxcar|flattopwin|gausswin|hanning|hamming|triang|sinc",
                   chosenFilter
                 )) {
                   handles$poleloc <- c(0L)
                   if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", chosenFilter,ignore.case=FALSE))) return() # info-system security
                   b <- try(eval(parse(text = chosenFilter)),silent=TRUE)
                   if (length(b) < 2L) {
                     handles$zeroloc <- c(0L)
                   }
                   else {
                     handles$zeroloc <- try(polyroot(rev(b)),silent=TRUE) # numerical stability may be an issue for all but low-degree polynomials
                   }
                 }
                 else if (grepl("FftFilter", chosenFilter)) {
                   handles$poleloc <- c(0L)
                   if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", chosenFilter,ignore.case=FALSE))) return() # info-system security
                   b <- try(eval(parse(text = chosenFilter)),silent=TRUE)
                   if (length(b) < 2L) {
                     handles$zeroloc <- c(0L)
                   }
                   else {
                     handles$zeroloc <- try(polyroot(rev(b)),silent=TRUE) # numerical stability may be an issue for all but low-degree polynomials
                   }
                 }
                 else {
                   if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", chosenFilter,ignore.case=FALSE))) return() # info-system security
                   a <- try(eval(parse(text = paste0(chosenFilter, "$a"))),silent=TRUE)
                   if (length(a) < 2L) {
                     handles$poleloc <- c(0L)
                   }
                   else {
                     handles$poleloc <- try(polyroot(rev(a)),silent=TRUE) # numerical stability may be an issue for all but low-degree polynomials
                   }
                   if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", chosenFilter,ignore.case=FALSE))) return() # info-system security
                   b <- try(eval(parse(text = paste0(chosenFilter, "$b"))),silent=TRUE)
                   if (length(b) < 2L) {
                     handles$zeroloc <- c(0L)
                   }
                   else {
                     handles$zeroloc <- try(polyroot(rev(b)),silent=TRUE) # numerical stability may be an issue for all but low-degree polynomials
                   }
                 }
                 updateNumericInput(session, inputId = "edit_gain",
                                    value = if (abs(b[1]) >= eps) { abs(b[1]) } else {
                   filtb <- handlesb()
                   filta <- handlesa()
                   filtb[Re(filtb) > 1e+12] <- 1e+12
                   filtb[Re(filtb) < -1e+12] <- -1e+12
                   filta[Re(filta) > 1e+12] <- 1e+12
                   filta[Re(filta) < -1e+12] <- -1e+12
                   filtb[Im(filtb) > 1e+12] <- Re(filtb) + 1i * 1e+12
                   filtb[Im(filtb) < -1e+12] <- Re(filtb) - 1i * 1e+12
                   filta[Im(filta) > 1e+12] <- Re(filtb) + 1i * 1e+12
                   filta[Im(filta) < -1e+12] <- Re(filtb) - 1i * 1e+12
                   rv <- signal::freqz(
                     filtb,
                     filta,
                     region = "whole",
                     n = 2L ^ 20L,
                     Fs = 2 * pi * input$samplingfreq / 2
                   )
                   1/ max(Re(rv$h),na.rm=TRUE)
                 }
                 ) # end updateNumericInput
                 
                 # updateCheckboxInput(session, inputId = "normalizedMagPlotAmplitude", value = TRUE) # why!?!?!?
                 
                 updateTextInput(
                   session,
                   inputId = "filenameExport",
                   value = gsub("[^[:alnum:]]",# http://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r
                                "", input$commonFilters)
                 )
                 updateTextInput(session,
                                 inputId = "headerlineExport",
                                 value = paste0("#",
                                                input$commonFilters))
                 updateTextInput(session,
                                 inputId = "edit_currentSelectionText",
                                 value = input$commonFilters)
                 updateSelectInput(session, inputId = "listbox_zero", choices = handles$zeroloc)
                 updateSelectInput(session, inputId = "listbox_pole", choices = handles$poleloc)
                 handlestol <- 10000
                 Myzeros <- handles$zeroloc
                 Mypoles <- handles$poleloc
                 pL <- length(Mypoles)
                 zL <- length(Myzeros)
                 if ((pL + zL) > 0L)
                   handles$connection <- diag(pL + zL)
                 handles$connecttype <- NULL
                 if (pL > 0L)
                   for (k in (1L:pL)) {
                     handles$connecttype <- c(handles$connecttype, "x")
                   }
                 if (zL > 0L)
                   for (k in ((pL + 1L):(pL + zL))) {
                     handles$connecttype <- c(handles$connecttype, "o")
                   }
                 if (!pracma::isempty(Mypoles)) {
                   for (ii in (1L:pL)) {
                     conjp <- which((round(handlestol * Mypoles) / handlestol) ==
                                      (Conj((
                                        round(handlestol * Mypoles[ii]) / handlestol
                                      ))))
                     if (!pracma::isempty(conjp)) {
                       handles$connection[ii, conjp] <- 1L
                       polezero1 <-
                         which(
                           round(handlestol * Myzeros) / handlestol ==
                             round(handlestol * Re((
                               1/ c(Mypoles[ii])
                             ))) / handlestol +
                             round(handlestol * Im((
                               1/ c(Mypoles[ii])
                             )) * (0 + 1i)) / handlestol
                         )
                       polezero2 <-
                         which(
                           round(handlestol * Myzeros) / handlestol ==
                             round(handlestol * Re((
                               1/ c(Mypoles[ii])
                             ))) / handlestol -
                             round(handlestol * Im((
                               1/ c(Mypoles[ii])
                             )) * (0 + 1i)) / handlestol
                         )
                       if (!pracma::isempty(polezero1)) {
                         handles$connection[ii, pL + polezero1] <- 1L
                         handles$connection[ii, pL + polezero2] <- 1L
                       }
                     }
                   }
                 }
                 if (!pracma::isempty(Myzeros)) {
                   for (ii in (pL + 1L):(pL + zL)) {
                     conjz <- which(round(handlestol * Myzeros) / handlestol ==
                                      Conj((
                                        round(handlestol * Myzeros[ii - pL]) / handlestol
                                      )))
                     if (!pracma::isempty(conjz)) {
                       handles$connection[ii, conjz + pL] <- 1L
                     }
                   }
                 }
               })
  output$filterCommand <- renderPrint({
    
  })
  observeEvent(eventExpr = input$logarithmicFreqAxis,
               handlerExpr = {
                 if (input$logarithmicFreqAxis) {
                   updateCheckboxInput(session, inputId = "twosidedFFT", value = FALSE)
                   updateCheckboxInput(session, inputId = "FFTshifted", value = FALSE)
                 }
               })
  observeEvent(eventExpr = input$FFTshifted, handlerExpr = {
    updateCheckboxInput(session, inputId = "twosidedFFT", value = input$FFTshifted)
  })
  observeEvent(eventExpr = input$logarithmicMagPlotAmplitude,
               handlerExpr = {
                 updateCheckboxInput(session,
                                     inputId = "normalizedMagPlotAmplitude",
                                     value = input$logarithmicMagPlotAmplitude)
                 if (input$logarithmicMagPlotAmplitude) {
                   handles$minZoomlimYpassband <- -9
                   handles$maxZoomlimYpassband <- 3
                   handles$stepZoomlimYpassband <- 0.1
                   newValueYpassband <- c(-3, 0)
                   handles$minZoomlimYstopband <- -160
                   handles$maxZoomlimYstopband <- -10
                   handles$stepZoomlimYstopband <- 1
                   newValueYstopband <- c(-130,-60)
                 }
                 else {
                   handles$minZoomlimYpassband <- -1
                   handles$maxZoomlimYpassband <- 100
                   handles$stepZoomlimYpassband <- 0.1
                   newValueYpassband <- c(0.5, 2)
                   handles$minZoomlimYstopband <- -0.1
                   handles$maxZoomlimYstopband <- 0.1
                   handles$stepZoomlimYstopband <- 0.005
                   newValueYstopband <- c(0, 0.05)
                 }
                 updateSliderInput(
                   session,
                   inputId = "zoomlimYpassband",
                   value = newValueYpassband,
                   min = handles$minZoomlimYpassband,
                   max = handles$maxZoomlimYpassband,
                   step = handles$stepZoomlimYpassband
                 )
                 updateSliderInput(
                   session,
                   inputId = "zoomlimYstopband",
                   value = newValueYstopband,
                   min = handles$minZoomlimYstopband,
                   max = handles$maxZoomlimYstopband,
                   step = handles$stepZoomlimYstopband
                 )
               })
  inputstretchyslider1step <- 0.01
  output$stretchyslider1rangeWidget <- renderUI({
    sliderInput(
      inputId = "stretchyslider1range",
      label = HTML("Ray min,max Limits  (-)&RightTee;&LeftTee;(+)"),
      min = handles$minslider1range,
      max = handles$maxslider1range,
      value = c(0L, 0.5) * input$samplingfreq / 2,
      step = inputstretchyslider1step,
      ticks = TRUE,
      animate = FALSE,
      sep = " ",
      dragRange = TRUE
    )
  })
  inputslider1animinterval <- 0
  output$slider1Widget <- renderUI({
    req(
      input$stretchyslider1range,
      inputstretchyslider1step,
      inputslider1animinterval
    )
    sliderInput(
      inputId = "slider1",
      label = HTML(paste0(
        "&uparrow; ",
        tags$span(style = "color:magenta;font-weight:bold", "Ray"),
        "-value (freq)"
      )),
      min = input$stretchyslider1range[1],
      max = input$stretchyslider1range[2],
      value = handles$slider1value,
      step = inputstretchyslider1step,
      ticks = TRUE,
      animate = animationOptions(interval = inputslider1animinterval, loop = FALSE),
      sep = " "
    )
  })
  output$stretchyslider2AstepWidget <- renderUI({
    sliderInput(
      inputId = "stretchyslider2Astep",
      label = "slider2A/ animation step-size",
      min = handles$minslider2Astep,
      max = handles$maxslider2Astep,
      value = 0.01,
      step = 1e-06,
      ticks = TRUE,
      animate = FALSE,
      sep = " "
    )
  })
  output$stretchyslider2ArangeWidget <- renderUI({
    sliderInput(
      inputId = "stretchyslider2Arange",
      label = HTML("min,max Limits2A  (-)&RightTee;&LeftTee;(+)"),
      min = handles$minslider2Arange,
      max = handles$maxslider2Arange,
      value = c(0L, 0.5),
      step = input$stretchyslider2Astep,
      ticks = TRUE,
      animate = FALSE,
      sep = " ",
      dragRange = TRUE
    )
  })
  output$slider2AanimintervalWidget <- renderUI({
    sliderInput(
      inputId = "slider2Aaniminterval",
      label = "animation-interval2A (msecs)",
      min = handles$minslider2Aaniminterval,
      max = handles$maxslider2Aaniminterval,
      value = 350,
      step = 50L,
      ticks = TRUE,
      animate = FALSE,
      sep = " "
    )
  })
  output$slider2AWidget <- renderUI({
    req(
      input$stretchyslider2Arange,
      input$stretchyslider2Astep,
      input$slider2Aaniminterval
    )
    sliderInput(
      inputId = "slider2A",
      label = HTML("val2A (real-part; or polar-radius)"),
      min = input$stretchyslider2Arange[1],
      max = input$stretchyslider2Arange[2],
      value = handles$slider2Avalue,
      step = input$stretchyslider2Astep,
      ticks = TRUE,
      animate = animationOptions(
        interval = input$slider2Aaniminterval,
        loop = FALSE
      ),
      sep = " "
    )
  })
  output$stretchyslider2BstepWidget <- renderUI({
    sliderInput(
      inputId = "stretchyslider2Bstep",
      label = "slider2B/ animation step-size",
      min = handles$minslider2Bstep,
      max = handles$maxslider2Bstep,
      value = 0.01,
      step = 1e-06,
      ticks = TRUE,
      animate = FALSE,
      sep = " "
    )
  })
  output$stretchyslider2BrangeWidget <- renderUI({
    sliderInput(
      inputId = "stretchyslider2Brange",
      label = HTML("min,max Limits2B  (-)&RightTee;&LeftTee;(+)"),
      min = handles$minslider2Brange,
      max = handles$maxslider2Brange,
      value = c(0L, 0.5),
      step = input$stretchyslider2Bstep,
      ticks = TRUE,
      animate = FALSE,
      sep = " ",
      dragRange = TRUE
    )
  })
  output$slider2BanimintervalWidget <- renderUI({
    sliderInput(
      inputId = "slider2Baniminterval",
      label = "animation-interval2B (msecs)",
      min = handles$minslider2Baniminterval,
      max = handles$maxslider2Baniminterval,
      value = 350,
      step = 50L,
      ticks = TRUE,
      animate = FALSE,
      sep = " "
    )
  })
  output$slider2BWidget <- renderUI({
    req(
      input$stretchyslider2Brange,
      input$stretchyslider2Bstep,
      input$slider2Baniminterval
    )
    sliderInput(
      inputId = "slider2B",
      label = HTML("val2B (imag-part; or polar-angle)"),
      min = input$stretchyslider2Brange[1],
      max = input$stretchyslider2Brange[2],
      value = handles$slider2Bvalue,
      step = input$stretchyslider2Bstep,
      ticks = TRUE,
      animate = animationOptions(
        interval = input$slider2Baniminterval,
        loop = FALSE
      ),
      sep = " "
    )
  })
  observeEvent(eventExpr = input$stretchyslider1range,
               handlerExpr = {
                 if (abs(input$stretchyslider1range[2] - handles$maxslider1range) <
                     0.04 * abs(handles$maxslider1range - handles$minslider1range)) {
                   handles$maxslider1range <- min(c(
                     round((
                       handles$maxslider1range +
                         (handles$maxslider1range - 1e-06) * handles$growSliders
                     ) / inputstretchyslider1step
                     ) *
                       inputstretchyslider1step,
                     round((
                       handles$maxslider1range +
                         (handles$maxslider1range - handles$minslider1range) * handles$growSliders
                     ) / inputstretchyslider1step
                     ) *
                       inputstretchyslider1step
                   ))
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider1range",
                     value = input$stretchyslider1range,
                     max = handles$maxslider1range
                   )
                   updateSliderInput(
                     session,
                     inputId = "slider1",
                     value = handles$slider1value,
                     max = input$stretchyslider1range[2]
                   )
                 }
                 if (abs(input$stretchyslider1range[1] - handles$minslider1range) <
                     0.04 * abs(handles$maxslider1range - handles$minslider1range)) {
                   handles$minslider1range <- round((
                     handles$minslider1range -
                       (handles$maxslider1range - handles$minslider1range) * handles$growSliders
                   ) / inputstretchyslider1step) *
                     inputstretchyslider1step
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider1range",
                     value = input$stretchyslider1range,
                     min = handles$minslider1range
                   )
                   updateSliderInput(
                     session,
                     inputId = "slider1",
                     value = handles$slider1value,
                     min = input$stretchyslider1range[1]
                   )
                 }
                 if (abs(input$stretchyslider1range[2] - input$stretchyslider1range[1]) <
                     0.1 * abs(handles$maxslider1range - handles$minslider1range)) {
                   if (mean(c(input$stretchyslider1range)) < mean(c(handles$maxslider1range,
                                                                    handles$minslider1range))) {
                     handles$maxslider1range <- max(
                       c(
                         input$stretchyslider1range[2] +
                           3L * inputstretchyslider1step,
                         round((
                           handles$maxslider1range -
                             (handles$maxslider1range - handles$minslider1range) *
                             handles$growSliders
                         ) / inputstretchyslider1step
                         ) * inputstretchyslider1step
                       )
                     ,na.rm=TRUE)
                     updateSliderInput(
                       session,
                       inputId = "stretchyslider1range",
                       value = input$stretchyslider1range,
                       max = handles$maxslider1range
                     )
                     updateSliderInput(
                       session,
                       inputId = "slider1",
                       value = handles$slider1value,
                       max = input$stretchyslider1range[2]
                     )
                   }
                   else {
                     handles$minslider1range <- min(
                       c(
                         input$stretchyslider1range[1] -
                           3L * inputstretchyslider1step,
                         round((
                           handles$minslider1range +
                             (handles$maxslider1range - handles$minslider1range) *
                             handles$growSliders
                         ) / inputstretchyslider1step
                         ) * inputstretchyslider1step
                       )
                     )
                     updateSliderInput(
                       session,
                       inputId = "stretchyslider1range",
                       value = input$stretchyslider1range,
                       min = handles$minslider1range
                     )
                     updateSliderInput(
                       session,
                       inputId = "slider1",
                       value = handles$slider1value,
                       min = input$stretchyslider1range[1]
                     )
                   }
                 }
                 handles$slider1value <- input$slider1
               })
  observeEvent(eventExpr = c(input$slider2A, input$slider2B),
               handlerExpr = {
                 if ((!is.null(handles$zeroloc)) && (!is.na(handles$zeroloc))) {
                   if ((is.null(input$listbox_zero)) || (is.na(input$listbox_zero))) {
                     tgt <- handles$zeroloc[length(handles$zeroloc)]
                   }
                   else {
                     tgt <- input$listbox_zero
                   }
                   matchpoint <- match(tgt, handles$zeroloc, nomatch = -1L)
                   if ((matchpoint > 0L) &&
                       (matchpoint <= length(handles$zeroloc))) {
                     if (input$tabPoleZeroEditing == "rtheta") {
                       if ((is.null(input$edit_currentSelectionText)) ||
                           (is.na(input$edit_currentSelectionText))) {
                         valueToReplace <- input$slider2A * exp(1i * input$slider2B)
                       }
                       else {
                         if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_currentSelectionText,ignore.case=FALSE))) return() # info-system security
                         valueToReplace <-
                           try(eval(parse(text = input$edit_currentSelectionText)),silent=TRUE)
                       }
                     }
                     else {
                       if ((is.null(input$edit_currentSelectionText)) ||
                           (is.na(input$edit_currentSelectionText))) {
                         n1 <- input$slider2A
                         n2 <- input$slider2B
                         if (is.infinite(Im(n2))) {
                           valueToReplace <- complex(1, n1, switch(sign(n2) +
                                                                     2L,-Inf, 0, Inf))
                         }
                         else if (is.infinite(Re(n2))) {
                           valueToReplace <- complex(1, n1, switch(sign(n2) +
                                                                     2L,-Inf, 0, Inf))
                         }
                         else {
                           valueToReplace <- n1 + 1i * n2
                         }
                       }
                       else {
                         if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_currentSelectionText,ignore.case=FALSE))) return() # info-system security
                         valueToReplace <-
                           try(eval(parse(text = input$edit_currentSelectionText)),silent=TRUE)
                       }
                     }
                   }
                   if ((Mod(valueToReplace) > 1e-06)) {
                     if (abs(Im(valueToReplace)) == 0L) { # isTRUE(all.equal( #
                       valueToReplace <- Re(valueToReplace)
                     }
                     handles$zeroloc[matchpoint] <- valueToReplace
                     updateSelectInput(
                       session,
                       inputId = "listbox_zero",
                       choices = handles$zeroloc,
                       selected = handles$zeroloc[matchpoint]
                     )
                   }
                   else {
                     showNotification(
                       ui = "Value is too small (or clear the edit-field of the zero-value)...",
                       duration = 3L,
                       closeButton = TRUE,
                       type = "message"
                     )
                   }
                 }
               })
  observeEvent(eventExpr = input$stretchyslider2Arange,
               handlerExpr = {
                 if (abs(input$stretchyslider2Arange[2] - handles$maxslider2Arange) <
                     0.04 * abs(handles$maxslider2Arange - handles$minslider2Arange)) {
                   handles$maxslider2Arange <- min(c(
                     round((
                       handles$maxslider2Arange +
                         (handles$maxslider2Arange - 1e-06) * handles$growSliders
                     ) / input$stretchyslider2Astep
                     ) *
                       input$stretchyslider2Astep,
                     round((
                       handles$maxslider2Arange +
                         (handles$maxslider2Arange - handles$minslider2Arange) *
                         handles$growSliders
                     ) / input$stretchyslider2Astep
                     ) * input$stretchyslider2Astep
                   ))
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Arange",
                     value = input$stretchyslider2Arange,
                     max = handles$maxslider2Arange
                   )
                   updateSliderInput(
                     session,
                     inputId = "slider2A",
                     value = handles$slider2Avalue,
                     max = input$stretchyslider2Arange[2]
                   )
                 }
                 if (abs(input$stretchyslider2Arange[1] - handles$minslider2Arange) <
                     0.04 * abs(handles$maxslider2Arange - handles$minslider2Arange)) {
                   handles$minslider2Arange <- round((
                     handles$minslider2Arange -
                       (handles$maxslider2Arange - handles$minslider2Arange) *
                       handles$growSliders
                   ) / input$stretchyslider2Astep
                   ) * input$stretchyslider2Astep
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Arange",
                     value = input$stretchyslider2Arange,
                     min = handles$minslider2Arange
                   )
                   updateSliderInput(
                     session,
                     inputId = "slider2A",
                     value = handles$slider2Avalue,
                     min = input$stretchyslider2Arange[1]
                   )
                 }
                 if (abs(input$stretchyslider2Arange[2] - input$stretchyslider2Arange[1]) <
                     0.1 * abs(handles$maxslider2Arange - handles$minslider2Arange)) {
                   if (mean(c(input$stretchyslider2Arange)) < mean(c(handles$maxslider2Arange,
                                                                     handles$minslider2Arange))) {
                     handles$maxslider2Arange <- max(
                       c(
                         input$stretchyslider2Arange[2] +
                           3L * input$stretchyslider2Astep,
                         round((
                           handles$maxslider2Arange -
                             (handles$maxslider2Arange - handles$minslider2Arange) *
                             handles$growSliders
                         ) / input$stretchyslider2Astep
                         ) *
                           input$stretchyslider2Astep
                       )
                     ,na.rm=TRUE)
                     updateSliderInput(
                       session,
                       inputId = "stretchyslider2Arange",
                       value = input$stretchyslider2Arange,
                       max = handles$maxslider2Arange
                     )
                     updateSliderInput(
                       session,
                       inputId = "slider2A",
                       value = handles$slider2Avalue,
                       max = input$stretchyslider2Arange[2]
                     )
                   }
                   else {
                     handles$minslider2Arange <- min(
                       c(
                         input$stretchyslider2Arange[1] -
                           3L * input$stretchyslider2Astep,
                         round((
                           handles$minslider2Arange +
                             (handles$maxslider2Arange - handles$minslider2Arange) *
                             handles$growSliders
                         ) / input$stretchyslider2Astep
                         ) *
                           input$stretchyslider2Astep
                       )
                     )
                     updateSliderInput(
                       session,
                       inputId = "stretchyslider2Arange",
                       value = input$stretchyslider2Arange,
                       min = handles$minslider2Arange
                     )
                     updateSliderInput(
                       session,
                       inputId = "slider2A",
                       value = handles$slider2Avalue,
                       min = input$stretchyslider2Arange[1]
                     )
                   }
                 }
                 handles$slider2Avalue <- input$slider2A
               })
  observeEvent(eventExpr = input$stretchyslider2Astep,
               handlerExpr = {
                 if (abs(input$stretchyslider2Astep - handles$maxslider2Astep) <
                     0.04 * abs(handles$maxslider2Astep - handles$minslider2Astep)) {
                   handles$maxslider2Astep <- round((
                     handles$maxslider2Astep +
                       (handles$maxslider2Astep - handles$minslider2Astep) * handles$growSliders
                   ) / 1e-06) *
                     1e-06
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Astep",
                     value = input$stretchyslider2Astep,
                     max = handles$maxslider2Astep
                   )
                   if (10L * handles$minslider2Astep < handles$maxslider2Astep / (1L +
                                                                                 handles$growSliders)) {
                     handles$minslider2Astep <- 10L * handles$minslider2Astep
                   }
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Astep",
                     value = input$stretchyslider2Astep,
                     min = handles$minslider2Astep
                   )
                 }
                 if (abs(input$stretchyslider2Astep - handles$minslider2Astep) <
                     0.04 * abs(handles$maxslider2Astep - handles$minslider2Astep)) {
                   handles$minslider2Astep <- max(c(1e-06, handles$minslider2Astep / 10),na.rm=TRUE)
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Astep",
                     value = input$stretchyslider2Astep,
                     min = handles$minslider2Astep
                   )
                   handles$maxslider2Astep <- max(c(
                     input$stretchyslider2Astep +
                       3L * 1e-06,
                     round((
                       handles$maxslider2Astep - (handles$maxslider2Astep -
                                                    handles$minslider2Astep) * handles$growSliders
                     ) / 1e-06
                     ) *
                       1e-06
                   ),na.rm=TRUE)
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Astep",
                     value = input$stretchyslider2Astep,
                     max = handles$maxslider2Astep
                   )
                 }
               })
  observeEvent(eventExpr = input$stretchyslider2Brange,
               handlerExpr = {
                 if (abs(input$stretchyslider2Brange[2] - handles$maxslider2Brange) <
                     0.04 * abs(handles$maxslider2Brange - handles$minslider2Brange)) {
                   handles$maxslider2Brange <- min(c(
                     round((
                       handles$maxslider2Brange +
                         (handles$maxslider2Brange - 1e-06) * handles$growSliders
                     ) / input$stretchyslider2Bstep
                     ) *
                       input$stretchyslider2Bstep,
                     round((
                       handles$maxslider2Brange +
                         (handles$maxslider2Brange - handles$minslider2Brange) *
                         handles$growSliders
                     ) / input$stretchyslider2Bstep
                     ) * input$stretchyslider2Bstep
                   ))
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Brange",
                     value = input$stretchyslider2Brange,
                     max = handles$maxslider2Brange
                   )
                   updateSliderInput(
                     session,
                     inputId = "slider2B",
                     value = handles$slider2Bvalue,
                     max = input$stretchyslider2Brange[2]
                   )
                 }
                 if (abs(input$stretchyslider2Brange[1] - handles$minslider2Brange) <
                     0.04 * abs(handles$maxslider2Brange - handles$minslider2Brange)) {
                   handles$minslider2Brange <- round((
                     handles$minslider2Brange -
                       (handles$maxslider2Brange - handles$minslider2Brange) *
                       handles$growSliders
                   ) / input$stretchyslider2Bstep
                   ) * input$stretchyslider2Bstep
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Brange",
                     value = input$stretchyslider2Brange,
                     min = handles$minslider2Brange
                   )
                   updateSliderInput(
                     session,
                     inputId = "slider2B",
                     value = handles$slider2Bvalue,
                     min = input$stretchyslider2Brange[1]
                   )
                 }
                 if (abs(input$stretchyslider2Brange[2] - input$stretchyslider2Brange[1]) <
                     0.1 * abs(handles$maxslider2Brange - handles$minslider2Brange)) {
                   if (mean(c(input$stretchyslider2Brange)) < mean(c(handles$maxslider2Brange,
                                                                     handles$minslider2Brange))) {
                     handles$maxslider2Brange <- max(
                       c(
                         input$stretchyslider2Brange[2] +
                           3L * input$stretchyslider2Bstep,
                         round((
                           handles$maxslider2Brange -
                             (handles$maxslider2Brange - handles$minslider2Brange) *
                             handles$growSliders
                         ) / input$stretchyslider2Bstep
                         ) *
                           input$stretchyslider2Bstep
                       )
                     ,na.rm=TRUE)
                     updateSliderInput(
                       session,
                       inputId = "stretchyslider2Brange",
                       value = input$stretchyslider2Brange,
                       max = handles$maxslider2Brange
                     )
                     updateSliderInput(
                       session,
                       inputId = "slider2B",
                       value = handles$slider2Bvalue,
                       max = input$stretchyslider2Brange[2]
                     )
                   }
                   else {
                     handles$minslider2Brange <- min(
                       c(
                         input$stretchyslider2Brange[1] -
                           3L * input$stretchyslider2Bstep,
                         round((
                           handles$minslider2Brange +
                             (handles$maxslider2Brange - handles$minslider2Brange) *
                             handles$growSliders
                         ) / input$stretchyslider2Bstep
                         ) *
                           input$stretchyslider2Bstep
                       )
                     )
                     updateSliderInput(
                       session,
                       inputId = "stretchyslider2Brange",
                       value = input$stretchyslider2Brange,
                       min = handles$minslider2Brange
                     )
                     updateSliderInput(
                       session,
                       inputId = "slider2B",
                       value = handles$slider2Bvalue,
                       min = input$stretchyslider2Brange[1]
                     )
                   }
                 }
                 handles$slider2Bvalue <- input$slider2B
               })
  observeEvent(eventExpr = input$stretchyslider2Bstep,
               handlerExpr = {
                 if (abs(input$stretchyslider2Bstep - handles$maxslider2Bstep) <
                     0.04 * abs(handles$maxslider2Bstep - handles$minslider2Bstep)) {
                   handles$maxslider2Bstep <- round((
                     handles$maxslider2Bstep +
                       (handles$maxslider2Bstep - handles$minslider2Bstep) * handles$growSliders
                   ) / 1e-06) *
                     1e-06
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Bstep",
                     value = input$stretchyslider2Bstep,
                     max = handles$maxslider2Bstep
                   )
                   if (10L * handles$minslider2Bstep < handles$maxslider2Bstep / (1L +
                                                                                 handles$growSliders)) {
                     handles$minslider2Bstep <- 10L * handles$minslider2Bstep
                   }
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Bstep",
                     value = input$stretchyslider2Bstep,
                     min = handles$minslider2Bstep
                   )
                 }
                 if (abs(input$stretchyslider2Bstep - handles$minslider2Bstep) <
                     0.04 * abs(handles$maxslider2Bstep - handles$minslider2Bstep)) {
                   handles$minslider2Bstep <- max(c(1e-06, handles$minslider2Bstep / 10),na.rm=TRUE)
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Bstep",
                     value = input$stretchyslider2Bstep,
                     min = handles$minslider2Bstep
                   )
                   handles$maxslider2Bstep <- max(c(
                     input$stretchyslider2Bstep +
                       3L * 1e-06,
                     round((
                       handles$maxslider2Bstep - (handles$maxslider2Bstep -
                                                    handles$minslider2Bstep) * handles$growSliders
                     ) / 1e-06
                     ) *
                       1e-06
                   ),na.rm=TRUE)
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Bstep",
                     value = input$stretchyslider2Bstep,
                     max = handles$maxslider2Bstep
                   )
                 }
               })
  output$zoomlimXpassbandWidget <- renderUI({
    sliderInput(
      inputId = "zoomlimXpassband",
      label = HTML("max X,Y-Limits, passband   (-)&RightTee;&LeftTee;(+)"),
      min = handles$minZoomlimXpassband,
      max = handles$maxZoomlimXpassband,
      value = c(0L, 0.5) * input$samplingfreq / 2,
      step = handles$stepZoomlimXpassband,
      round = -2L,
      ticks = TRUE,
      animate = animationOptions(interval = 300,
                                 loop = FALSE),
      sep = " ",
      dragRange = TRUE
    )
  })
  output$zoomlimYpassbandWidget <- renderUI({
    sliderInput(
      inputId = "zoomlimYpassband",
      label = NULL,
      min = handles$minZoomlimYpassband,
      max = handles$maxZoomlimYpassband,
      value = c(-3L, 0L),
      step = handles$stepZoomlimYpassband,
      round = -2L,
      ticks = TRUE,
      animate = animationOptions(interval = 300,
                                 loop = FALSE),
      sep = " ",
      dragRange = TRUE
    )
  })
  output$zoomlimXstopbandWidget <- renderUI({
    sliderInput(
      inputId = "zoomlimXstopband",
      label = HTML("max X,Y-Limits, stopband   (-)&RightTee;&LeftTee;(+)"),
      min = handles$minZoomlimXstopband,
      max = handles$maxZoomlimXstopband,
      value = c(0.5, 1L) * input$samplingfreq / 2,
      step = handles$stepZoomlimXstopband,
      ticks = TRUE,
      animate = animationOptions(interval = 300, loop = FALSE),
      sep = " ",
      dragRange = TRUE
    )
  })
  output$zoomlimYstopbandWidget <- renderUI({
    sliderInput(
      inputId = "zoomlimYstopband",
      label = NULL,
      min = handles$minZoomlimYstopband,
      max = handles$maxZoomlimYstopband,
      value = c(-130L,-60L),
      step = handles$stepZoomlimYstopband,
      round = 1L,
      ticks = TRUE,
      animate = animationOptions(interval = 300,
                                 loop = FALSE),
      sep = " ",
      dragRange = TRUE
    )
  })
  observeEvent(eventExpr = input$zoomlimXpassband,
               handlerExpr = {
                 if (abs(input$zoomlimXpassband[2] - handles$maxZoomlimXpassband) <
                     handles$stepZoomlimXpassband / 2) {
                   handles$maxZoomlimXpassband <- round((
                     handles$maxZoomlimXpassband +
                       (
                         handles$maxZoomlimXpassband - handles$minZoomlimXpassband
                       ) *
                       handles$growSliders
                   ) / handles$stepZoomlimXpassband
                   ) *
                     handles$stepZoomlimXpassband
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXpassband",
                     value = input$zoomlimXpassband,
                     max = handles$maxZoomlimXpassband
                   )
                 }
                 if (abs(input$zoomlimXpassband[1] - handles$minZoomlimXpassband) <
                     handles$stepZoomlimXpassband / 2) {
                   handles$minZoomlimXpassband <- round((
                     handles$minZoomlimXpassband -
                       (
                         handles$maxZoomlimXpassband - handles$minZoomlimXpassband
                       ) *
                       handles$growSliders
                   ) / handles$stepZoomlimXpassband
                   ) *
                     handles$stepZoomlimXpassband
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXpassband",
                     value = input$zoomlimXpassband,
                     min = handles$minZoomlimXpassband
                   )
                 }
                 if (abs(input$zoomlimXpassband[2] - input$zoomlimXpassband[1]) <
                     0.1 * abs(handles$maxZoomlimXpassband - handles$minZoomlimXpassband)) {
                   if (mean(c(input$zoomlimXpassband)) < mean(c(
                     handles$maxZoomlimXpassband,
                     handles$minZoomlimXpassband
                   ))) {
                     handles$maxZoomlimXpassband <- max(
                       input$zoomlimXpassband[2] +
                         handles$stepZoomlimXpassband,
                       round((
                         handles$maxZoomlimXpassband -
                           (
                             handles$maxZoomlimXpassband - handles$minZoomlimXpassband
                           ) *
                           handles$growSliders
                       ) / handles$stepZoomlimXpassband
                       ) *
                         handles$stepZoomlimXpassband
                     ,na.rm=TRUE)
                     updateSliderInput(
                       session,
                       inputId = "zoomlimXpassband",
                       value = input$zoomlimXpassband,
                       max = handles$maxZoomlimXpassband
                     )
                   }
                   else {
                     handles$minZoomlimXpassband <- min(
                       input$zoomlimXpassband[1] -
                         handles$stepZoomlimXpassband,
                       round((
                         handles$minZoomlimXpassband +
                           (
                             handles$maxZoomlimXpassband - handles$minZoomlimXpassband
                           ) *
                           handles$growSliders
                       ) / handles$stepZoomlimXpassband
                       ) *
                         handles$stepZoomlimXpassband
                     )
                     updateSliderInput(
                       session,
                       inputId = "zoomlimXpassband",
                       value = input$zoomlimXpassband,
                       min = handles$minZoomlimXpassband
                     )
                   }
                 }
               })
  observeEvent(eventExpr = input$zoomlimYpassband,
               handlerExpr = {
                 if (abs(input$zoomlimYpassband[2] - handles$maxZoomlimYpassband) <
                     handles$stepZoomlimYpassband / 2) {
                   handles$maxZoomlimYpassband <- round((
                     handles$maxZoomlimYpassband +
                       (
                         handles$maxZoomlimYpassband - handles$minZoomlimYpassband
                       ) *
                       handles$growSliders
                   ) / handles$stepZoomlimYpassband
                   ) *
                     handles$stepZoomlimYpassband
                   updateSliderInput(
                     session,
                     inputId = "zoomlimYpassband",
                     value = input$zoomlimYpassband,
                     max = handles$maxZoomlimYpassband
                   )
                 }
                 if (abs(input$zoomlimYpassband[1] - handles$minZoomlimYpassband) <
                     handles$stepZoomlimYpassband / 2) {
                   handles$minZoomlimYpassband <- round((
                     handles$minZoomlimYpassband -
                       (
                         handles$maxZoomlimYpassband - handles$minZoomlimYpassband
                       ) *
                       handles$growSliders
                   ) / handles$stepZoomlimYpassband
                   ) *
                     handles$stepZoomlimYpassband
                   updateSliderInput(
                     session,
                     inputId = "zoomlimYpassband",
                     value = input$zoomlimYpassband,
                     min = handles$minZoomlimYpassband
                   )
                 }
                 if (abs(input$zoomlimYpassband[2] - input$zoomlimYpassband[1]) <
                     0.1 * abs(handles$maxZoomlimYpassband - handles$minZoomlimYpassband)) {
                   if (mean(c(input$zoomlimYpassband)) < mean(c(
                     handles$maxZoomlimYpassband,
                     handles$minZoomlimYpassband
                   ))) {
                     handles$maxZoomlimYpassband <- max(
                       input$zoomlimYpassband[2] +
                         handles$stepZoomlimYpassband,
                       round((
                         handles$maxZoomlimYpassband -
                           (
                             handles$maxZoomlimYpassband - handles$minZoomlimYpassband
                           ) *
                           handles$growSliders
                       ) / handles$stepZoomlimYpassband
                       ) *
                         handles$stepZoomlimYpassband
                     ,na.rm=TRUE)
                     updateSliderInput(
                       session,
                       inputId = "zoomlimYpassband",
                       value = input$zoomlimYpassband,
                       max = handles$maxZoomlimYpassband
                     )
                   }
                   else {
                     handles$minZoomlimYpassband <- min(
                       input$zoomlimYpassband[1] -
                         handles$stepZoomlimYpassband,
                       round((
                         handles$minZoomlimYpassband +
                           (
                             handles$maxZoomlimYpassband - handles$minZoomlimYpassband
                           ) *
                           handles$growSliders
                       ) / handles$stepZoomlimYpassband
                       ) *
                         handles$stepZoomlimYpassband
                     )
                     updateSliderInput(
                       session,
                       inputId = "zoomlimYpassband",
                       value = input$zoomlimYpassband,
                       min = handles$minZoomlimYpassband
                     )
                   }
                 }
               })
  observeEvent(eventExpr = input$zoomlimXstopband,
               handlerExpr = {
                 if (abs(input$zoomlimXstopband[2] - handles$maxZoomlimXstopband) <
                     handles$stepZoomlimXstopband / 2) {
                   handles$maxZoomlimXstopband <- round((
                     handles$maxZoomlimXstopband +
                       (
                         handles$maxZoomlimXstopband - handles$minZoomlimXstopband
                       ) *
                       handles$growSliders
                   ) / handles$stepZoomlimXstopband
                   ) *
                     handles$stepZoomlimXstopband
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXstopband",
                     value = input$zoomlimXstopband,
                     max = handles$maxZoomlimXstopband
                   )
                 }
                 if (abs(input$zoomlimXstopband[1] - handles$minZoomlimXstopband) <
                     handles$stepZoomlimXstopband / 2) {
                   handles$minZoomlimXstopband <- round((
                     handles$minZoomlimXstopband -
                       (
                         handles$maxZoomlimXstopband - handles$minZoomlimXstopband
                       ) *
                       handles$growSliders
                   ) / handles$stepZoomlimXstopband
                   ) *
                     handles$stepZoomlimXstopband
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXstopband",
                     value = input$zoomlimXstopband,
                     min = handles$minZoomlimXstopband
                   )
                 }
                 if (abs(input$zoomlimXstopband[2] - input$zoomlimXstopband[1]) <
                     0.1 * abs(handles$maxZoomlimXstopband - handles$minZoomlimXstopband)) {
                   if (mean(c(input$zoomlimXstopband)) < mean(c(
                     handles$maxZoomlimXstopband,
                     handles$minZoomlimXstopband
                   ))) {
                     handles$maxZoomlimXstopband <- max(
                       input$zoomlimXstopband[2] +
                         handles$stepZoomlimXstopband,
                       round((
                         handles$maxZoomlimXstopband -
                           (
                             handles$maxZoomlimXstopband - handles$minZoomlimXstopband
                           ) *
                           handles$growSliders
                       ) / handles$stepZoomlimXstopband
                       ) *
                         handles$stepZoomlimXstopband
                     ,na.rm=TRUE)
                     updateSliderInput(
                       session,
                       inputId = "zoomlimXstopband",
                       value = input$zoomlimXstopband,
                       max = handles$maxZoomlimXstopband
                     )
                   }
                   else {
                     handles$minZoomlimXstopband <- min(
                       input$zoomlimXstopband[1] -
                         handles$stepZoomlimXstopband,
                       round((
                         handles$minZoomlimXstopband +
                           (
                             handles$maxZoomlimXstopband - handles$minZoomlimXstopband
                           ) *
                           handles$growSliders
                       ) / handles$stepZoomlimXstopband
                       ) *
                         handles$stepZoomlimXstopband
                     )
                     updateSliderInput(
                       session,
                       inputId = "zoomlimXstopband",
                       value = input$zoomlimXstopband,
                       min = handles$minZoomlimXstopband
                     )
                   }
                 }
               })
  observeEvent(eventExpr = input$zoomlimYstopband,
               handlerExpr = {
                 if (abs(input$zoomlimYstopband[2] - handles$maxZoomlimYstopband) <
                     handles$stepZoomlimYstopband / 2) {
                   handles$maxZoomlimYstopband <- round((
                     handles$maxZoomlimYstopband +
                       (
                         handles$maxZoomlimYstopband - handles$minZoomlimYstopband
                       ) *
                       handles$growSliders
                   ) / handles$stepZoomlimYstopband
                   ) *
                     handles$stepZoomlimYstopband
                   updateSliderInput(
                     session,
                     inputId = "zoomlimYstopband",
                     value = input$zoomlimYstopband,
                     max = handles$maxZoomlimYstopband
                   )
                 }
                 if (abs(input$zoomlimYstopband[1] - handles$minZoomlimYstopband) <
                     handles$stepZoomlimYstopband / 2) {
                   handles$minZoomlimYstopband <- round((
                     handles$minZoomlimYstopband -
                       (
                         handles$maxZoomlimYstopband - handles$minZoomlimYstopband
                       ) *
                       handles$growSliders
                   ) / handles$stepZoomlimYstopband
                   ) *
                     handles$stepZoomlimYstopband
                   updateSliderInput(
                     session,
                     inputId = "zoomlimYstopband",
                     value = input$zoomlimYstopband,
                     min = handles$minZoomlimYstopband
                   )
                 }
                 if (abs(input$zoomlimYstopband[2] - input$zoomlimYstopband[1]) <
                     0.1 * abs(handles$maxZoomlimYstopband - handles$minZoomlimYstopband)) {
                   if (mean(c(input$zoomlimYstopband)) < mean(c(
                     handles$maxZoomlimYstopband,
                     handles$minZoomlimYstopband
                   ))) {
                     handles$maxZoomlimYstopband <- max(
                       input$zoomlimYstopband[2] +
                         handles$stepZoomlimYstopband,
                       round((
                         handles$maxZoomlimYstopband -
                           (
                             handles$maxZoomlimYstopband - handles$minZoomlimYstopband
                           ) *
                           handles$growSliders
                       ) / handles$stepZoomlimYstopband
                       ) *
                         handles$stepZoomlimYstopband
                     ,na.rm=TRUE)
                     updateSliderInput(
                       session,
                       inputId = "zoomlimYstopband",
                       value = input$zoomlimYstopband,
                       max = handles$maxZoomlimYstopband
                     )
                   }
                   else {
                     handles$minZoomlimYstopband <- min(
                       input$zoomlimYstopband[1] -
                         handles$stepZoomlimYstopband,
                       round((
                         handles$minZoomlimYstopband +
                           (
                             handles$maxZoomlimYstopband - handles$minZoomlimYstopband
                           ) *
                           handles$growSliders
                       ) / handles$stepZoomlimYstopband
                       ) *
                         handles$stepZoomlimYstopband
                     )
                     updateSliderInput(
                       session,
                       inputId = "zoomlimYstopband",
                       value = input$zoomlimYstopband,
                       min = handles$minZoomlimYstopband
                     )
                   }
                 }
               })
  observeEvent(eventExpr = input$freqaxisunits,
               handlerExpr = {
                 if (input$freqaxisunits == "zero2one") {
                   updateNumericInput(session, inputId = "samplingfreq", value = 2.0)
                   handles$minZoomlimXpassband <- 0.0
                   handles$maxZoomlimXpassband <- 1.0
                   handles$minZoomlimXstopband <- 0.0
                   handles$maxZoomlimXstopband <- 1.0
                   handles$minslider1range <- 0.0
                   handles$maxslider1range <- 1.0
                   slider1value <- 0.5
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXpassband",
                     value = c(0.0,
                               0.5),
                     min = 0.0,
                     max = handles$maxZoomlimXpassband,
                     step = 0.05
                   )
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXstopband",
                     value = c(0.5,
                               1.0),
                     min = 0.0,
                     max = 1.0,
                     step = 0.05
                   )
                 }
                 else if (input$freqaxisunits == "zero2pi") {
                   updateNumericInput(session, inputId = "samplingfreq", 
                                      value = 2 * pi)
                   handles$maxZoomlimXpassband <- pi
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXpassband",
                     value = c(0.0,
                               pi / 2),
                     min = 0L,
                     max = handles$maxZoomlimXpassband,
                     step = pi / 12
                   )
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXstopband",
                     value = c(0.0,
                               pi / 2),
                     min = 0L,
                     max = pi,
                     step = pi / 12
                   )
                 }
                 else if (input$freqaxisunits == "zero22pi") {
                   updateNumericInput(session, inputId = "samplingfreq", 
                                      value = 4L * pi)
                   updateCheckboxInput(session, inputId = "twosidedFFT", value = FALSE)
                   updateCheckboxInput(session, inputId = "FFTshifted", value = FALSE)
                 }
                 else if (input$freqaxisunits == "zero2half") {
                   updateNumericInput(session,
                                      inputId = "samplingfreq",
                                      value = 1L * input$samplingfreq / 2)
                 }
                 else if (input$freqaxisunits == "zero2piby2") {
                   updateNumericInput(session, inputId = "samplingfreq", value = pi)
                 }
                 else if ((input$freqaxisunits == "zero2fs") ||
                          (input$freqaxisunits == "zero2fsby2")
                          ) {
                   updateNumericInput(session, inputId = "samplingfreq", value = 44100)
                   handles$minZoomlimXpassband <- 0L
                   handles$maxZoomlimXpassband <- 44100 / 2
                   handles$minZoomlimXstopband <- 0L
                   handles$maxZoomlimXstopband <- 44100 / 2
                   handles$minslider1range <- 0L
                   handles$maxslider1range <- 44100
                   slider1value <- (44100 / 2) / 2
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXpassband",
                     value = c(0L,
                               0.5) * input$samplingfreq /
                       2L,
                     min = 0L,
                     max = handles$maxZoomlimXpassband,
                     step = 0.05
                   )
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXstopband",
                     value = c(0.5,
                               1L) * input$samplingfreq /
                       2L,
                     min = 0L,
                     max = input$samplingfreq / 2,
                     step = 0.05
                   )
                 }
                 else if (input$freqaxisunits == "zero2fmax") {
                   updateNumericInput(session,
                                      inputId = "samplingfreq",
                                      value = input$maxfreqsampled / 2)
                   updateCheckboxInput(session, inputId = "twosidedFFT", value = FALSE)
                   updateCheckboxInput(session, inputId = "FFTshifted", value = FALSE)
                 }
                 else if (input$freqaxisunits == "zero2fmaxby2") {
                   updateNumericInput(session,
                                      inputId = "samplingfreq",
                                      value = input$maxfreqsampled / 2)
                 }
               })
  output$coordsfilecontentsPrint2Import <- renderPrint({
    inpFile <- input$filecoordsImport
    if (is.null(inpFile))
      return(NULL)
    if (input$textbinaryformatImport == "csv") {
      rawList <- readLines(con = inpFile$name) # inpFile$datapath) # input$filecoordsImport$datapath)
      Npoles <- match("#zerolocs", rawList) - match("#polelocs",
                                                    rawList) - 1L
      importedPoles <- scan(
        file = inpFile$name, # inpFile$datapath, # input$filecoordsImport$datapath,
        n = Npoles,
        quiet = TRUE,
        what = if (input$coordsevaluateImport) {
          character()
        }
        else {
          complex()
        },
        comment.char = "#",
        sep = "\t"
      )
      importedZeros <-
        scan(
          file = inpFile$name, # inpFile$datapath, # input$filecoordsImport$datapath,
          skip = match("#zerolocs",
                       rawList),
          quiet = TRUE,
          what = if (input$coordsevaluateImport) {
            character()
          }
          else {
            complex()
          },
          comment.char = "#",
          sep = "\t"
        )
      maxlengthimported <-
        max(length(importedPoles), length(importedZeros),na.rm=TRUE)
       # http://stackoverflow.com/questions/19074163/cbind-is-there-a-way-to-have-missing-values-set-to-na
      length(importedPoles) <- maxlengthimported
      length(importedZeros) <- maxlengthimported
      print(cbind(Zeros = importedZeros, Poles = importedPoles),
            na.print = "")
      # close(inpFile$datapath)
    }
    else if (input$textbinaryformatImport == "bin") {
      # as a quick-check:
      # cat(file=stderr(),"input$filecoordsImport$datapath:", input$filecoordsImport$datapath,".\n")
      zz <- file(description = inpFile$name, open = "rb") # input$filecoordsImport$datapath, open = "rb")
      print(readBin(
            con = zz,
            what = complex(),
            n = 20L # (maximal) number of records to be read
          ))
      close(zz)
    }
    else if (input$textbinaryformatImport == "RData") {
      # as a quick-check:
      # cat(file=stderr(),input$filecoordsImport$datapath,".\n")
      load(file = inpFile$name) # input$filecoordsImport$datapath #, verbose = TRUE) # inpFile$datapath, verbose = TRUE)
      print(cbind(Zeros = myzeroscopy, Poles = mypolescopy),
                  na.print = "")
    }
    else if (input$textbinaryformatImport == "mat") {
      # as a quick-check:
      # cat(file=stderr(),input$filecoordsImport$datapath,".\n")
      data <- R.matlab::readMat(con = inpFile$name) # input$filecoordsImport$datapath) # inpFile$datapath) # Returns a named-list structure containing all variables in the MAT file structure
      print(data)
    }
    else if (input$textbinaryformatImport == "yml") {
      # cat(file=stderr(),input$filecoordsImport$datapath,".\n")
      data <- yaml::yaml.load_file(input = inpFile$name) # input$filecoordsImport$datapath) # inpFile$datapath)
      print(cbind(Zeros=data$myzeroscopy, Poles=data$mypolescopy),
            na.print = "")
    }
    unlink(input$filecoordsImport$datapath) # inpFile$datapath)
  }) # end output$coordsfilecontentsPrint2Import
  
  observeEvent(eventExpr = c(input$pzplot_dblclick),
               handlerExpr = {
                 Myhover <- input$pzplot_hover
                 Mydf <-
                   as.data.frame(cbind(xReal = c(
                     Re(handles$poleloc), Re(handles$zeroloc)
                   ),
                   yImag = c(
                     Im(handles$poleloc), Im(handles$zeroloc)
                   )), row.names = c(paste0("p",
                                            1L:length(
                                              handles$poleloc
                                            )), paste0("z", 1L:length(
                                              handles$zeroloc
                                            ))))
                 point <-
                   nearPoints(
                     df = Mydf,
                     coordinfo = input$pzplot_dblclick,
                     xvar = "xReal",
                     yvar = "yImag",
                     threshold = 7L,
                     maxpoints = 1L,
                     addDist = TRUE
                   )
                 if (!(handles$inDragMode) && (NROW(point) == 0L))
                   return(NULL)
                 if (handles$inDragMode) {
                   handles$inDragMode <- FALSE
                   if (input$tb_zoomenable) {
                     
                   }
                   else {
                     
                   }
                   if (!(input$tb_addrepeatedly)) {
                     if (input$tb_clickenable) {
                       
                     }
                     else {
                       handles$selectedpole <- list(XData = NULL, YData = NULL)
                       handles$selectedzero <- list(XData = NULL, YData = NULL)
                     }
                   }
                   else if (!(input$tb_addrepeatedly) && (handles$changed)) {
                     if (input$tb_clickenable) {
                       
                     }
                     else {
                       handles$selectedpole <- list(XData = NULL, YData = NULL)
                       handles$selectedzero <- list(XData = NULL, YData = NULL)
                     }
                   }
                   handles$changed <- FALSE
                 }
                 else {
                   showNotification(
                     ui = paste0("Point [", point[1L, 1], ", ",
                                 point[1L, 2], "j] is now selected..."),
                     duration = 3L,
                     closeButton = TRUE,
                     type = "warning"
                   )
                   handles$inDragMode <- TRUE
                   currentpt <- c(input$pzplot_hover$x, input$pzplot_hover$y)
                   x <- currentpt[1]
                   y <- currentpt[2]
                   if ("real_motionT" %in% input$DragLockGrp) {
                     y <- 0L
                   }
                   if ("circular_motionT" %in% input$LockCoordsPolarGrp) {
                     handles$circular_radius <- sqrt(x * x + y * y)
                     currenttheta <- atan2(y, x)
                     x <- handles$circular_radius * cos(currenttheta)
                     y <- handles$circular_radius * sin(currenttheta)
                     handleslinerc$xdata <- handles$circular_radius * sin(seq(0L,
                                                                              2 * pi, by = 2 * pi /
                                                                                20L))
                     handleslinerc$ydata <- handles$circular_radius * cos(seq(0L,
                                                                              2 * pi, by = 2 * pi /
                                                                                20L))
                   }
                   if ("radial_motionT" %in% input$LockCoordsPolarGrp) {
                     handles$radial_theta <- atan2(y, x)
                     r <- sqrt(x * x + y * y)
                     z <- c(cos(handles$radial_theta),
                            sin(handles$radial_theta))
                     handleslinerr$xdata <- c(0L, 2L * z[1])
                     handleslinerr$ydata <- c(0L, 2L * z[2])
                     x <- r * z[1]
                     y <- r * z[2]
                   }
                   Mydf <- as.data.frame(cbind(xReal = c(
                     Re(handles$poleloc),
                     Re(handles$zeroloc)
                   ), yImag = c(
                     Im(handles$poleloc), Im(handles$zeroloc)
                   )),
                   row.names = c(paste0("p", 1L:length(
                     handles$poleloc
                   )), paste0("z",
                              1L:length(
                                handles$zeroloc
                              ))))
                   closestRowsToClickpoint <-
                     nearPoints(
                       df = Mydf,
                       coordinfo = input$pzplot_hover,
                       xvar = "xReal",
                       yvar = "yImag",
                       threshold = 7L
                     )
                   if (!pracma::isempty(closestRowsToClickpoint)) {
                     if ((NROW(closestRowsToClickpoint) > 0L) &&
                         (startsWith(row.names(closestRowsToClickpoint)[1],
                                     "z"))) {
                       indexOfClickedPoleOrZeroWithinEntireSet <-
                         pracma::str2num(substring(
                           row.names(closestRowsToClickpoint[1L,]),
                           first = 2L,
                           last = nchar(closestRowsToClickpoint[1L,])
                         )) + length(handles$poleloc)
                       handles$subscriptOfClickedPoleOrZero <-
                         indexOfClickedPoleOrZeroWithinEntireSet -
                         length(handles$poleloc)
                       polestring <- which(handles$connecttype == "x")
                       zerostring <- which(handles$connecttype == "o")
                       theseAreConnected <-
                         which(handles$connection[zerostring[handles$subscriptOfClickedPoleOrZero],] != 0L)
                       theseAreConnected2 <-
                         setdiff(intersect(zerostring, theseAreConnected),
                                 zerostring[indexOfClickedPoleOrZeroWithinEntireSet])
                       outputParametersFromStartdragToMovepez_zeroconnect <- NULL
                       if (!pracma::isempty(theseAreConnected2)) {
                         outputParametersFromStartdragToMovepez_zeroconnect <-
                           which(zerostring ==
                                   sort(setdiff(
                                     intersect(zerostring, theseAreConnected),
                                     zerostring[indexOfClickedPoleOrZeroWithinEntireSet]
                                   )))
                       }
                       temppoleNumber <-
                         sort(intersect(polestring, theseAreConnected))
                       poleconnect <- NULL
                       if (!pracma::isempty(temppoleNumber)) {
                         for (i in (1L:(length(temppoleNumber)))) {
                           poleconnect <- c(poleconnect,
                                            which(polestring ==
                                                    temppoleNumber(i)))
                         }
                       }
                       else {
                         poleconnect <- NULL
                       }
                     }
                   }
                 }
               })
  observeEvent(eventExpr = c(input$pzplot_hover),
               handlerExpr = {
                 if (handles$inDragMode) {
                   currentpt <- c(input$pzplot_hover$x, input$pzplot_hover$y)
                   x <- currentpt[1]
                   y <- currentpt[2]
                   if ("real_motionT" %in% input$DragLockGrp) {
                     y <- 0L
                   }
                   if ("circular_motionT" %in% input$LockCoordsPolarGrp) {
                     handles$circular_radius <- sqrt(x * x + y * y)
                     currenttheta <- atan2(y, x)
                     x <- handles$circular_radius * cos(currenttheta)
                     y <- handles$circular_radius * sin(currenttheta)
                     handleslinerc$xdata <- handles$circular_radius * sin(seq(0L,
                                                                              2 * pi, by = 2 * pi /
                                                                                20L))
                     handleslinerc$ydata <- handles$circular_radius * cos(seq(0L,
                                                                              2 * pi, by = 2 * pi /
                                                                                20L))
                   }
                   if ("radial_motionT" %in% input$LockCoordsPolarGrp) {
                     handles$radial_theta <- atan2(y, x)
                     r <- sqrt(x * x + y * y)
                     z <- c(cos(handles$radial_theta),
                            sin(handles$radial_theta))
                     handleslinerr$xdata <- c(0L, 2L * z[1])
                     handleslinerr$ydata <- c(0L, 2L * z[2])
                     x <- r * z[1]
                     y <- r * z[2]
                   }
                   Mydf <- as.data.frame(cbind(xReal = c(
                     Re(handles$poleloc),
                     Re(handles$zeroloc)
                   ), yImag = c(
                     Im(handles$poleloc), Im(handles$zeroloc)
                   )),
                   row.names = c(paste0("p", 1L:length(
                     handles$poleloc
                   )), paste0("z",
                              1L:length(
                                handles$zeroloc
                              ))))
                   closestRowsToClickpoint <-
                     nearPoints(
                       df = Mydf,
                       coordinfo = input$pzplot_hover,
                       xvar = "xReal",
                       yvar = "yImag",
                       threshold = 7L
                     )
                   str(closestRowsToClickpoint)
                   if (!(pracma::isempty(closestRowsToClickpoint)) &&
                       (!anyNA(closestRowsToClickpoint)) &&
                       (!("NA" %in% row.names(closestRowsToClickpoint)[1]))) {
                     if ((NROW(closestRowsToClickpoint) > 0L) &&
                         (startsWith(row.names(closestRowsToClickpoint)[1],
                                     "z"))) {
                       indexOfClickedPoleOrZeroWithinEntireSet <-
                         pracma::str2num(substring(
                           row.names(closestRowsToClickpoint[1L,]),
                           first = 2L,
                           last = nchar(closestRowsToClickpoint[1L,])
                         )) + length(handles$poleloc)
                       handles$subscriptOfClickedPoleOrZero <-
                         indexOfClickedPoleOrZeroWithinEntireSet -
                         length(handles$poleloc)
                       polestring <- which(handles$connecttype == "x")
                       zerostring <- which(handles$connecttype == "o")
                       theseAreConnected <-
                         which(handles$connection[zerostring[handles$subscriptOfClickedPoleOrZero],] != 0L)
                       theseAreConnected2 <-
                         setdiff(intersect(zerostring, theseAreConnected),
                                 zerostring[indexOfClickedPoleOrZeroWithinEntireSet])
                       outputParametersFromStartdragToMovepez_zeroconnect <- NULL
                       if (!pracma::isempty(theseAreConnected2)) {
                         outputParametersFromStartdragToMovepez_zeroconnect <-
                           which(zerostring ==
                                   sort(setdiff(
                                     intersect(zerostring, theseAreConnected),
                                     zerostring[indexOfClickedPoleOrZeroWithinEntireSet]
                                   )))
                       }
                       temppoleNumber <-
                         sort(intersect(polestring, theseAreConnected))
                       poleconnect <- NULL
                       if (!pracma::isempty(temppoleNumber)) {
                         for (i in (1L:(length(temppoleNumber)))) {
                           poleconnect <- c(poleconnect,
                                            which(polestring ==
                                                    temppoleNumber(i)))
                         }
                       }
                       else {
                         poleconnect <- NULL
                       }
                       try()
                       if (pracma::isempty(poleconnect)) {
                         subscriptOfClickedPoleOrZero <- handles$subscriptOfClickedPoleOrZero
                         handles$zeroloc[subscriptOfClickedPoleOrZero] <- x +
                           y * (0 + 1i)
                         tempstring <- handles$zeroloc
                         tempstring <-
                           c(tempstring[1L:(subscriptOfClickedPoleOrZero -
                                             1L)], x + y * (0 + 1i), tempstring[seq(
                                               from = (subscriptOfClickedPoleOrZero +
                                                         1L),
                                               to = length(tempstring)
                                             )])
                         handles$selectedzero <- list(XData = x, YData = y)
                         handles$selectedpole <- list(XData = NULL, YData = NULL)
                         if (!pracma::isempty(outputParametersFromStartdragToMovepez_zeroconnect)) {
                           z1 <- outputParametersFromStartdragToMovepez_zeroconnect[1]
                           handles$zeroloc[z1] <- x - y * (0 + 1i)
                           tempstring <-
                             c(tempstring[1L:(z1 - 1L)], x - y * (0 + 1i),
                               tempstring[seq(from = (z1 + 1L),
                                              to = length(tempstring))])
                           handles$selectedzero <-
                             list(XData = c(x, x), YData = c(y,-y))
                         }
                         updateSelectInput(session, inputId = "listbox_zero",
                                           choices = tempstring)
                         subscriptOfClickedPoleOrZero <- input$listbox_zero
                         list_str <-
                           handles$zeroloc[subscriptOfClickedPoleOrZero]
                         updateSelectizeInput(
                           session,
                           inputId = "edit_polezeroloc",
                           choices = c(input$edit_polezeroloc, list_str)
                         )
                       }
                       else {
                         handlestol <- 0.01
                         outputParametersFromStartdragToMovepez_poleconnect <-
                           poleconnect
                         if (Mod(Im((1/ c(
                           handles$zeroloc[handles$subscriptOfClickedPoleOrZero]
                         ))) -
                         Im(handles$poleloc[outputParametersFromStartdragToMovepez_poleconnect[1]])) <
                         handlestol) {
                           outputParametersFromStartdragToMovepez_type <- 1L
                         }
                         else {
                           outputParametersFromStartdragToMovepez_type <- 0L
                         }
                         if ((x ^ 2L + y ^ 2L) != 1L) {
                           x <- x + 1e-06
                         }
                         tempx <- Re(1/ c(x + y * (0 + 1i)))
                         tempy <- Im(1/ c(x + y * (0 + 1i)))
                         subscriptOfClickedPoleOrZero <-
                           handles$subscriptOfClickedPoleOrZero
                         handles$zeroloc[subscriptOfClickedPoleOrZero] <- x +
                           y * (0 + 1i)
                         tempstring <- handles$zeroloc
                         tempstring <-
                           c(tempstring[1L:(subscriptOfClickedPoleOrZero -
                                             1L)], x + y * (0 + 1i), tempstring[seq(
                                               from = (subscriptOfClickedPoleOrZero +
                                                         1L),
                                               to = length(tempstring)
                                             )])
                         handles$selectedzero <- list(XData = x, YData = y)
                         if (!pracma::isempty(outputParametersFromStartdragToMovepez_zeroconnect)) {
                           z1 <- outputParametersFromStartdragToMovepez_zeroconnect[1]
                           handles$zeroloc[z1] <- x - y * (0 + 1i)
                           tempstring <-
                             c(tempstring[1L:(z1 - 1L)], x + y * (0 + 1i),
                               tempstring[seq(from = (z1 + 1L),
                                              to = length(tempstring))])
                           handles$selectedzero <-
                             list(XData = c(x, x), YData = c(y,-y))
                         }
                         updateSelectInput(session, inputId = "listbox_zero",
                                           choices = tempstring)
                         tempstring <- handles$poleloc
                         if (outputParametersFromStartdragToMovepez_type ==
                             1L) {
                           p1 <- outputParametersFromStartdragToMovepez_poleconnect[1]
                           handles$poleloc[p1] <- tempx + tempy * (0 + 1i)
                           tempstring <-
                             c(tempstring[1L:(p1 - 1L)], tempx + tempy *
                                 (0 + 1i), tempstring[seq(from = (p1 + 1L),
                                                          to = length(tempstring))])
                           handles$selectedpole <-
                             list(XData = tempx, YData = tempy)
                           if (length(outputParametersFromStartdragToMovepez_poleconnect) >
                               1L) {
                             p2 <- outputParametersFromStartdragToMovepez_poleconnect[2]
                             handles$poleloc[p2] <- tempx - tempy * (0 + 1i)
                             tempstring <- c(tempstring[1L:(p2 - 1L)], tempx -
                                               tempy * (0 + 1i), tempstring[seq(from = (p2 + 1L),
                                                                                to = length(tempstring))])
                             handles$selectedpole <-
                               list(XData = c(tempx, tempx),
                                    YData = c(tempy,-tempy))
                           }
                         }
                         else {
                           p1 <- outputParametersFromStartdragToMovepez_poleconnect[1]
                           handles$poleloc[p1] <- tempx - tempy * (0 + 1i)
                           tempstring <-
                             c(tempstring[1L:(p1 - 1L)], tempx - tempy *
                                 (0 + 1i), tempstring[seq(from = (p1 + 1L),
                                                          to = length(tempstring))])
                           handles$selectedpole <-
                             list(XData = tempx, YData = -tempy)
                           if (length(outputParametersFromStartdragToMovepez_poleconnect) >
                               1L) {
                             p2 <- outputParametersFromStartdragToMovepez_poleconnect[2]
                             handles$poleloc[p2] <- tempx + tempy * (0 + 1i)
                             tempstring <- c(tempstring[1L:(p2 - 1L)], tempx +
                                               tempy * (0 + 1i), tempstring[seq(from = (p2 + 1L),
                                                                                to = length(tempstring))])
                             handles$selectedpole <-
                               list(XData = c(tempx, tempx),
                                    YData = c(tempy,-tempy))
                           }
                         }
                         updateSelectInput(session, inputId = "listbox_pole",
                                           choices = tempstring)
                       }
                     }
                     else if ((NROW(closestRowsToClickpoint) > 0L) &&
                              (startsWith(row.names(closestRowsToClickpoint)[1],
                                          "p"))) {
                       indexOfClickedPoleOrZeroWithinEntireSet <-
                         pracma::str2num(substring(
                           row.names(closestRowsToClickpoint[1L,]),
                           first = 2L,
                           last = nchar(closestRowsToClickpoint[1L,])
                         ))
                       handles$subscriptOfClickedPoleOrZero <-
                         indexOfClickedPoleOrZeroWithinEntireSet
                       zerostring <- which(handles$connecttype == "o")
                       polestring <- which(handles$connecttype == "x")
                       theseAreConnected <-
                         which(handles$connection[polestring[indexOfClickedPoleOrZeroWithinEntireSet],] != 0L)
                       temppoleNumber <-
                         setdiff(intersect(polestring, theseAreConnected),
                                 polestring[indexOfClickedPoleOrZeroWithinEntireSet])
                       outputParametersFromStartdragToMovepez_poleconnect <- NULL
                       if (!pracma::isempty(temppoleNumber)) {
                         outputParametersFromStartdragToMovepez_poleconnect <-
                           which(polestring ==
                                   sort(setdiff(
                                     intersect(polestring, theseAreConnected),
                                     polestring[indexOfClickedPoleOrZeroWithinEntireSet]
                                   )))
                       }
                       theseAreConnected2 <-
                         sort(intersect(zerostring, theseAreConnected))
                       zeroconnect <- NULL
                       if (!pracma::isempty(theseAreConnected2)) {
                         for (i in (1L:(length(theseAreConnected2)))) {
                           zeroconnect <- c(zeroconnect,
                                            which(zerostring ==
                                                    theseAreConnected2(i)))
                         }
                       }
                       else {
                         zeroconnect <- NULL
                       }
                       if (pracma::isempty(zeroconnect)) {
                         if ((x ^ 2L + y ^ 2L) != 1L) {
                           x <- x + 1e-06
                         }
                         subscriptOfClickedPoleOrZero <-
                           handles$subscriptOfClickedPoleOrZero
                         handles$poleloc[subscriptOfClickedPoleOrZero] <- x +
                           y * (0 + 1i)
                         tempstring <- handles$poleloc
                         tempstring <-
                           c(tempstring[1L:(subscriptOfClickedPoleOrZero -
                                             1L)], x + y * (0 + 1i), tempstring[seq(
                                               from = (subscriptOfClickedPoleOrZero +
                                                         1L),
                                               to = length(tempstring)
                                             )])
                         handles$selectedpole <- list(XData = x, YData = y)
                         handles$selectedzero <- list(XData = NULL, YData = NULL)
                         if (!pracma::isempty(outputParametersFromStartdragToMovepez_poleconnect)) {
                           p1 <- outputParametersFromStartdragToMovepez_poleconnect[1]
                           handles$poleloc[p1] <- x - y * (0 + 1i)
                           tempstring <-
                             c(tempstring[1L:(p1 - 1L)], x - y * (0 + 1i),
                               tempstring[seq(from = (p1 + 1L),
                                              to = length(tempstring))])
                           handles$selectedpole <-
                             list(XData = c(x, x), YData = c(y,-y))
                         }
                         updateSelectInput(session, inputId = "listbox_pole",
                                           choices = tempstring)
                         subscriptOfClickedPoleOrZero <- input$listbox_pole
                         list_str <-
                           handles$poleloc[subscriptOfClickedPoleOrZero]
                         updateSelectizeInput(
                           session,
                           inputId = "edit_polezeroloc",
                           choices = c(input$edit_polezeroloc, list_str)
                         )
                       }
                       else {
                         handlestol <- 0.01
                         outputParametersFromStartdragToMovepez_zeroconnect <-
                           zeroconnect
                         if (Mod(Im((1/ c(
                           handles$poleloc[handles$subscriptOfClickedPoleOrZero]
                         ))) -
                         Im(handles$zeroloc[outputParametersFromStartdragToMovepez_zeroconnect[1]])) <
                         handlestol) {
                           outputParametersFromStartdragToMovepez_type <- 1L
                         }
                         else {
                           outputParametersFromStartdragToMovepez_type <- 0L
                         }
                         if ((x ^ 2L + y ^ 2L) != 1L) {
                           x <- x + 1e-06
                         }
                         tempx <- Re(1/ c(x + y * (0 + 1i)))
                         tempy <- Im(1/ c(x + y * (0 + 1i)))
                         subscriptOfClickedPoleOrZero <-
                           handles$subscriptOfClickedPoleOrZero
                         handles$poleloc[subscriptOfClickedPoleOrZero] <- x +
                           y * (0 + 1i)
                         tempstring <- handles$poleloc
                         tempstring <-
                           c(tempstring[1L:(subscriptOfClickedPoleOrZero -
                                             1L)], x + y * (0 + 1i), tempstring[seq(
                                               from = (subscriptOfClickedPoleOrZero +
                                                         1L),
                                               to = length(tempstring)
                                             )])
                         handles$selectedpole <- list(XData = x, YData = y)
                         if (!pracma::isempty(outputParametersFromStartdragToMovepez_poleconnect)) {
                           p1 <- outputParametersFromStartdragToMovepez_poleconnect[1]
                           handles$poleloc[p1] <- x - y * (0 + 1i)
                           tempstring <-
                             c(tempstring[1L:(p1 - 1L)], x - y * (0 + 1i),
                               tempstring[seq(from = (p1 + 1L),
                                              to = length(tempstring))])
                           handles$selectedpole <-
                             list(XData = c(x, x), YData = c(y,-y))
                         }
                         updateSelectInput(session, inputId = "listbox_pole",
                                           choices = tempstring)
                         tempstring <- handles$zeroloc
                         if (outputParametersFromStartdragToMovepez_type ==
                             1L) {
                           z1 <- outputParametersFromStartdragToMovepez_zeroconnect[1]
                           handles$zeroloc[z1] <- tempx + tempy * (0 + 1i)
                           tempstring <-
                             c(tempstring[1L:(z1 - 1L)], tempx + tempy *
                                 (0 + 1i), tempstring[seq(from = (z1 + 1L),
                                                          to = length(tempstring))])
                           handles$selectedzero <-
                             list(XData = tempx, YData = tempy)
                           if (length(outputParametersFromStartdragToMovepez_zeroconnect) >
                               1L) {
                             z2 <- outputParametersFromStartdragToMovepez_zeroconnect[2]
                             handles$zeroloc[z2] <- tempx - tempy * (0 + 1i)
                             tempstring <- c(tempstring[1L:(z2 - 1L)], tempx -
                                               tempy * (0 + 1i), tempstring[seq(from = (z2 + 1L),
                                                                                to = length(tempstring))])
                             handles$selectedzero <-
                               list(XData = c(tempx, tempx),
                                    YData = c(tempy,-tempy))
                           }
                         }
                         else {
                           z1 <- outputParametersFromStartdragToMovepez_zeroconnect[1]
                           handles$zeroloc[z1] <- tempx - tempy * (0 + 1i)
                           tempstring <-
                             c(tempstring[1L:(z1 - 1L)], tempx - tempy *
                                 (0 + 1i), tempstring[seq(from = (z1 + 1L),
                                                          to = length(tempstring))])
                           handles$selectedzero <-
                             list(XData = tempx, YData = -tempy)
                           if (length(outputParametersFromStartdragToMovepez_zeroconnect) >
                               1L) {
                             z2 <- outputParametersFromStartdragToMovepez_zeroconnect[2]
                             handles$zeroloc[z2] <- tempx + tempy * (0 + 1i)
                             tempstring <- c(tempstring[1L:(z2 - 1L)], tempx +
                                               tempy * (0 + 1i), tempstring[seq(from = (z2 + 1L),
                                                                                to = length(tempstring))])
                             handles$selectedzero <-
                               list(XData = c(tempx, tempx),
                                    YData = c(tempy,-tempy))
                           }
                         }
                         updateSelectInput(session, inputId = "listbox_zero",
                                           choices = tempstring)
                       }
                     }
                   }
                 }
               })
  
  # output$report - downloadHandler ----
  output$report <-
    downloadHandler(
      filename = function() {
        paste0("report",
        if (input$IncludeTimeStampExport) {
          paste0("-", Sys.Date())
        },
        switch(input$reportFormat,
               html=".html",
               pdf=".pdf",
               latex=".tex",
               slidy=".html",
               word=".doc",
               beamer=".pdf",
               ioslides=".html"
               )
        )
        },
      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        writeLines(
          con = tempReport,
          text = paste0(
            # CREATE TEMPLATE report.Rmd ----
            # https://shiny.rstudio.com/articles/generating-reports.html

'---
title: "Dynamic Report 112"
author: ', 
'_(PeZdemoR)_', # rmarkdown::metadata$author,
'
date: "`r Sys.Date()`"
output:
', 
        switch(input$reportFormat,
html='  html_document:
    number_sections: yes
    toc: yes
    toc_float: no
    theme: readable
    highlight: textmate
    toc_depth: 4
    code_folding: hide
    fig_caption: yes
    self_contained: yes',
pdf='  pdf_document:
    fig_caption: yes
    highlight: default
    keep_tex: no
    number_sections: yes
    toc: yes
    toc_depth: 4',
latex='  pdf_document:
    fig_caption: yes
    highlight: default
    keep_tex: yes
    number_sections: yes
    toc: yes
    toc_depth: 4',
# https://www.w3.org/Talks/Tools/Slidy2/Overview.html
slidy='  slidy_presentation:
    duration: 80
    fig_caption: yes
    footer: Copyright (c) 2017
    keep_md: yes
    highlight: default
    incremental: yes
    self_contained: yes',
word='  word_document:
    fig_caption: yes
    highlight: default
    keep_md: yes
    toc: yes
    toc_depth: 4',
# https://www.hartwork.org/beamer-theme-matrix/
beamer='  beamer_presentation:
    colortheme: beaver
    fig_caption: yes
    keep_tex: yes
    theme: Singapore
    toc: yes
    incremental: no
    fig_height: 7
    fig_width: 10
    fonttheme: professionalfonts
    highlight: default',
# http://rmarkdown.rstudio.com/ioslides_presentation_format.html
ioslides='  ioslides_presentation: 
    fig_caption: yes
    highlight: default
    incremental: yes
    keep_md: yes
    smaller: no
    widescreen: no
    transition: default
    self_contained: yes
runtime: shiny'
),
'
params:
  n: 50
  inputcommonFilters: "(template)"
  handlesb: !r c(1,1)
  handlesa: !r c(1,1)
  handleshn: !r c(1,1)
  zeroloc: !r c(0.5,0.6)
  poleloc: !r c(0.5,0.6)
  normalizedMagPlotAmplitude: TRUE
  ForegroundColor: "blue"
  LineWidth: 3
  FFTshifted: TRUE
  unwrapPhase: FALSE
  twosidedFFT: TRUE
  samplingfreq: 44100
  showPhaseOnMagPlot: FALSE
  logarithmicFreqAxis: FALSE
  grcolor: "#BCBCBC"
  freqaxisunits: "zero2one"
  checkboxRAY: TRUE
  slider1: 1
  secondaryaxis: TRUE
  logarithmicMagPlotAmplitude: TRUE
  BackgroundColor: "transparent"
  edit_gain: 1
  showLegend: FALSE
  showMaxMinsOnMagPlot: TRUE
  magnminimumsa: !r c(-100,-80)
  magnminimumsf: !r c(0,0.9)
  magnmaximumsa: !r c(-10,-20)
  magnmaximumsf: !r c(0,0.9)
  zoomlimXpassband: !r c(0,0.5)
  zoomlimYpassband: !r c(-3,0)
  zoomlimXstopband: !r c(0.5,1)
  zoomlimYstopband: !r c(-130,-60)
  includeSourceCode: TRUE
  appwd: !r getwd()
  minimumLogFreqDisplayed: 0.001
  todaysdate: !r Sys.Date()
---

https://shiny.rstudio.com/articles/generating-reports.html

***

',
            input$myPrefaceNote,
'

\```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = params$includeSourceCode) # global chunk-option for whether to include R source-code into the output-file
\```

# The Start

\```{r eval = FALSE, echo = FALSE}
# Notes:
#
# Note that due to an issue in `rmarkdown`, the default-value of 
# any parameter within the header cannot be `NUL`, so we used a default 
# of `NA` for the default-value of all parameters.
#
\```

\```{r}
# ... et voila!: the `params` object-list is now available inside the document.
params$n          # derived from somewhere external to this document
params$todaysdate # default internal-value, or just replace with external/ fixed one
\```

***

# The Middle Paragraph

Well, here it is:

***

## The Plot

A plot of $n=`r params$n`$ quantity of 2D normally-distributed random-points.

\```{r, fig.cap="This is the Plot."}
plot(rnorm(params$n), rnorm(params$n))
grid(); abline(h=0L); abline(v=0L); box(which="figure")
\```

***

\```{r, fig.cap="Magnitude Plot"}
library(signal)
library(pracma)
assign("j", 0 + (0 + 1i))
assign("1j", 0 + (0 + 1i))
assign("Infi", complex(1, 0, Inf))
assign("Infj", complex(1, 0, Inf))
assign("eps", .Machine$double.neg.eps)
assign("twoeps", .Machine$double.eps)
assign("fpmaxx", .Machine$double.xmax)

myfindpeaks <-
  function(x,
           nups = 1L,
           ndowns = nups,
           zero = "0",
           peakpat = NULL,
           minpeakheight = -Inf,
           minpeakdistance = 1L,
           threshold = 0,
           npeaks = 0L,
           sortstr = FALSE) {
    stopifnot(is.vector(x, mode = "numeric"))
    if (!zero %in% c("0", "+", "-"))
      stop("Argument `zero` can only be `0`, `+`, or `-`.")
    xc <- paste(as.character(sign(diff(x))), collapse = "")
    xc <- gsub("1", "+", gsub("-1", "-", xc))
    if (zero != "0")
      xc <- gsub("0", zero, xc)
    if (is.null(peakpat)) {
      peakpat <- sprintf("[+]{%d,}[-]{%d,}", nups, ndowns)
    }
    rc <- gregexpr(peakpat, xc)[[1]]
    if (rc[1] < 0L)
      return(NULL)
    x1 <- rc
    x2 <- rc + attr(rc, "match.length")
    attributes(x1) <- NULL
    attributes(x2) <- NULL
    n <- length(x1)
    xv <- xp <- numeric(n)
    for (i in 1L:n) {
      xp[i] <- which.max(x[x1[i]:x2[i]]) + x1[i] - 1L
      xv[i] <- x[xp[i]]
    }
    inds <-
      which(xv >= minpeakheight & xv - pmax(x[x1], x[x2]) >= threshold)
    X <- cbind(xv[inds], xp[inds], x1[inds], x2[inds])
    if (minpeakdistance < 1L)
      warning("Handling `minpeakdistance < 1`` is logically not possible.")
    if (sortstr || minpeakdistance > 1L) {
      sl <- sort.list(X[, 1], na.last = NA, decreasing = TRUE)
      X <- X[sl, , drop = FALSE]
    }
    if (length(X) == 0L)
      return(c())
    if (minpeakdistance > 1L) {
      no_peaks <- nrow(X)
      badpeaks <- rep(FALSE, no_peaks)
      for (i in 1L:no_peaks) {
        ipos <- X[i, 2]
        if (!badpeaks[i]) {
          dpos <- abs(ipos - X[, 2])
          badpeaks <- badpeaks | (dpos > 0L & dpos < minpeakdistance)
        }
      }
      X <- X[!badpeaks,]
    }
    if (is.vector(X)) {
      if (npeaks > 0L && npeaks < length(X) / 4) {
        X <- X[1L:npeaks, , drop = FALSE]
      }
    }
    else {
      if (npeaks > 0L && npeaks < nrow(X)) {
        X <- X[1L:npeaks, , drop = FALSE]
      }
    }
    return(X)
           }

#axes_mag renderPlot, replicated
filtb <- params$handlesb
filta <- params$handlesa
filtb[Re(filtb) > 1e+12] <- 1e+12
filtb[Re(filtb) < -1e+12] <- -1e+12
filta[Re(filta) > 1e+12] <- 1e+12
filta[Re(filta) < -1e+12] <- -1e+12
filtb[Im(filtb) > 1e+12] <- Re(filtb) + 1i * 1e+12
filtb[Im(filtb) < -1e+12] <- Re(filtb) - 1i * 1e+12
filta[Im(filta) > 1e+12] <- Re(filtb) + 1i * 1e+12
filta[Im(filta) < -1e+12] <- Re(filtb) - 1i * 1e+12
rv <-
  signal::freqz(
    filtb,
    filta,
    region = "whole",
    n = 2 ^ 20,
    Fs = 2 *
      pi * params$samplingfreq / 2
  )
par(mgp = c(2.5, 1, 0))
plot(
  if (params$FFTshifted) {
    (rv$f / pi - params$samplingfreq / 2)
  }
  else {
    if (params$twosidedFFT) {
      rv$f / (2 * pi)
    }
    else {
      rv$f / (pi)
    }
  },
  if (params$logarithmicMagPlotAmplitude) {
    (if (params$FFTshifted) {
      20 * log10(Mod(pracma::fftshift(rv$h)))
    }
    else {
      20 * log10(Mod(rv$h))
    }) - (if (params$normalizedMagPlotAmplitude) {
      20 * log10(max(Mod(rv$h), na.rm = TRUE))
    }
    else {
      20 * log10(1/ params$edit_gain)
    })
  }
  else {
    (if (params$FFTshifted) {
      Mod(pracma::fftshift(rv$h))
    }
    else {
      Mod(rv$h)
    }) / (if (params$normalizedMagPlotAmplitude) {
      max(Mod(rv$h), na.rm = TRUE)
    }
    else {
      1/ params$edit_gain
    })
  },
  log = if (params$logarithmicFreqAxis) {
    "x"
  }
  else {
    ""
  },
  xlim = if (params$logarithmicFreqAxis) {
    c(max(c(params$minimumLogFreqDisplayed, 1e-04 * params$samplingfreq / 2),na.rm=TRUE),
      params$samplingfreq / 2)
  }
  else {
    c(if (params$twosidedFFT) {
      -params$samplingfreq / 2
    } else {
      0L
    }, if (params$FFTshifted) {
      params$samplingfreq / 2
    } else {
      if (params$twosidedFFT) {
        params$samplingfreq / 2
      } else {
        2L * params$samplingfreq / 2
      }
    })
  },
  ylim = if (params$logarithmicMagPlotAmplitude) {
    c(max(-130L, min(-20L, 20 * log10(
      min(Mod(rv$h), na.rm = TRUE) / (if (params$normalizedMagPlotAmplitude) {
        max(Mod(rv$h), na.rm = TRUE)
      } else {
        1/ params$edit_gain
      })
    )),na.rm=TRUE), max(0L, min(120, 20 * log10(
      max(Mod(rv$h), na.rm = TRUE) / (if (params$normalizedMagPlotAmplitude) {
        max(Mod(rv$h), na.rm = TRUE)
      } else {
        1/ params$edit_gain
      })
    ), na.rm = TRUE),na.rm=TRUE))
  }
  else {
    c(0L, max(1L, Mod(rv$h) / (if (params$normalizedMagPlotAmplitude) {
      max(Mod(rv$h), na.rm = TRUE)
    } else {
      1/ params$edit_gain
    }), na.rm = TRUE))
  },
  type = "l",
  col = params$ForegroundColor,
  bg = params$BackgroundColor,
  lwd = params$LineWidth,
  xlab = if (params$freqaxisunits == "zero2one") {
    expression("Normalized Frequency (" %*% pi ~ ~ "[rads/Sample])")
  }
  else if (params$freqaxisunits == "zero2pi") {
    expression(omega ~ ~ "[rads]")
  }
  else if (params$freqaxisunits == "zero22pi") {
    expression(omega ~ ~ "[rads]")
  }
  else if (params$freqaxisunits == "zero2half") {
    expression(omega / {
      2 * pi
    } ~ ~  ~  ~ "[rads]")
  }
  else if (params$freqaxisunits == "zero2piby2") {
    expression(2L * omega ~ ~  ~  ~ "[rads]")
  }
  else if (params$freqaxisunits == "zero2fs") {
    if (params$twosidedFFT)
      expression(-f[s] %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
    else
      expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
  }
  else if (params$freqaxisunits == "zero2fsby2") {
    if ((!params$twosidedFFT) &&
        (!params$FFTshifted) && (!params$logarithmicFreqAxis)) {
      expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
    }
    else {
      expression(0L %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
    }
  }
  else if (params$freqaxisunits == "zero2fmax") {
    expression(0L %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
  }
  else if (params$freqaxisunits == "zero2fmaxby2") {
    if (params$twosidedFFT)
      expression(-f[max] / 2 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
    else
      expression(0L %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
  },
  ylab = if ((params$freqaxisunits == "zero2one") ||
             (params$freqaxisunits ==
              "zero2pi") ||
             (params$freqaxisunits == "zero22pi") || (params$freqaxisunits ==
                                                     "zero2half") ||
             (params$freqaxisunits == "zero2piby2")) {
    if (params$normalizedMagPlotAmplitude) {
      if (params$logarithmicMagPlotAmplitude) {
        expression(paste(group("|", bold(H(
          e ^ {
            1i * omega
          }
        )), "|"), " / max [dB]"))
      }
      else {
        expression(paste(group("|", bold(H(
          e ^ {
            1i * omega
          }
        )), "|")), " / max")
      }
    }
    else {
      if (params$logarithmicMagPlotAmplitude) {
        expression(paste(group("|", bold(H(
          e ^ {
            1i * omega
          }
        )), "|"), "  [dB]"))
      }
      else {
        expression(group("|", bold(H(e ^ {
          1i * omega
        })), "|"))
      }
    }
  }
  else {
    if (params$logarithmicMagPlotAmplitude) {
      expression(paste(group("|", bold(H(
        e ^ {
          j ~ ~ 2 * pi * f
        }
      )), "|"), "[dB]"))
    }
    else {
      expression(group("|", bold(H(e ^ {
        j ~ ~ 2 * pi * f
      })), "|"))
    }
  },
  main = paste0("Magnitude-Reponse", if (params$normalizedMagPlotAmplitude) {
    " (normalized)"
  })
)
if (params$checkboxRAY)
  abline(
    v = params$slider1,
    lwd = params$LineWidth,
    col = "magenta",
    lty = "dashed"
  )
grid(col = params$grcolor)
abline(h = 0L, col = "black")
abline(v = 0L, col = "black")
if (params$secondaryaxis) {
  if ((params$freqaxisunits == "zero2pi") || (params$freqaxisunits ==
                                             "zero22pi") ||
      (params$freqaxisunits == "zero2piby2")) {
    axis(
      side = 3L,
      at=pi*c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
      labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
      tick = TRUE,
      pos = par("usr")[4],
      outer = FALSE,
      col = params$grcolor,
      cex.axis = 0.8,
      mgp = c(-2.5,-1L,0L),
      xlab = "pi-based"
    )
  }
  if ((params$freqaxisunits == "zero2one") ||
      (params$freqaxisunits ==
       "zero2half")) {
    axis(
      side = 3L,
      at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
      labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
      tick = TRUE,
      pos = par("usr")[4],
      outer = FALSE,
      col = params$grcolor,
      cex.axis = 0.8,
      mgp = c(-2.5,-1L,0L)
    )
  }
  if ((params$freqaxisunits == "zero2fs") ||
      (params$freqaxisunits ==
       "zero2fmax") ||
      (params$freqaxisunits == "zero2fmaxby2")) {
    axis(
      side = 3L,
      at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L
      ) * params$samplingfreq / 2,
      labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
      tick = TRUE,
      line = -0.4,
      pos = par("usr")[4],
      outer = FALSE,
      col = params$grcolor,
      cex.axis = 0.8,
      mgp = c(-2.5,-1L, 0L)
    )
  }
}
if (params$showPhaseOnMagPlot) {
  lines(
    if (params$FFTshifted) {
      (rv$f / pi - params$samplingfreq / 2)
    }
    else {
      if (params$twosidedFFT) {
        rv$f / (2 * pi)
      }
      else {
        rv$f / (pi)
      }
    },
    if (params$unwrapPhase) {
      if (params$FFTshifted) {
        unWrap(Arg(pracma::fftshift(rv$h)), tol = (pi - eps))
      }
      else {
        unWrap(Arg(rv$h), tol = (pi - eps))
      }
    }
    else {
      if (params$FFTshifted) {
        Arg(pracma::fftshift(rv$h))
      }
      else {
        Arg(rv$h)
      }
    },
    col = params$grcolor,
    lwd = params$LineWidth * 2/3,
    lty = "dotted"
  )
  if (params$showLegend)
    legend(
      "topright",
      inset = 0.03,
      c("magn.", "phase"),
      col = c(params$ForegroundColor,
              "grey"),
      lty = c("solid", "dotted"),
      bty = "n",
      cex = 0.8
    )
}
if (params$showMaxMinsOnMagPlot == TRUE) {
  x <-
    myfindpeaks(
      Mod(rv$h),
      minpeakheight = 1e-12,
      minpeakdistance = (length(rv$h) / 100L) / 2,
      threshold = 0.001,
      npeaks = 100L
    )
  xmins <-
    myfindpeaks(
      -Mod(rv$h),
      minpeakdistance = (length(rv$h) / 100L) / 2,
      threshold = 0.001,
      npeaks = 100L
    )
  if (!pracma::isempty(x)) {
    if (!(is.vector(x))) {
      x <- x[order(x[, 2]),]
    }
    if (is.vector(x)) {

    }
    else {

    }
    if (is.vector(x)) {
      if (params$FFTshifted) {
        paramsmagnmaximumsf <- (rv$f[x[2]] / pi)
      }
      else {
        if (params$twosidedFFT) {
          paramsmagnmaximumsf <- rv$f[x[2]] / (2 * pi)
        }
        else {
          paramsmagnmaximumsf <- rv$f[x[2]] / (pi)
        }
      }
    }
    else {
      if (params$FFTshifted) {
        paramsmagnmaximumsf <- (rv$f[x[, 2]] / pi)
      }
      else {
        if (params$twosidedFFT) {
          paramsmagnmaximumsf <- rv$f[x[, 2]] / (2 * pi)
        }
        else {
          paramsmagnmaximumsf <- rv$f[x[, 2]] / (pi)
        }
      }
    }
    if (is.vector(x)) {
      if (params$logarithmicMagPlotAmplitude) {
        if (params$FFTshifted) {
          paramsmagnmaximumsa <- 20 * log10(x[1])
        }
        else {
          paramsmagnmaximumsa <- 20 * log10(x[1])
        }
        if (!(params$normalizedMagPlotAmplitude)) {
          indxs <- which(params$magnmaximumsa < (20 * log10(max(Mod(rv$h), na.rm = TRUE
                                                             )) - params$minimumLogFreqDisplayed))
          paramsmagnmaximumsa <-
            params$magnmaximumsa[indxs]
          paramsmagnmaximumsf <-
            params$magnmaximumsf[indxs]
        }
        if (params$normalizedMagPlotAmplitude) {
          paramsmagnmaximumsa <- params$magnmaximumsa -
            20 * log10(max(Mod(rv$h), na.rm = TRUE))
        }
        else {
          paramsmagnmaximumsa <- params$magnmaximumsa -
            20 * log10(1/ params$edit_gain)
        }
      }
      else {
        if (params$FFTshifted) {
          paramsmagnmaximumsa <- x[1]
        }
        else {
          paramsmagnmaximumsa <- x[1]
        }
        if (!(params$normalizedMagPlotAmplitude)) {
          indxs <- which(params$magnmaximumsa < max(Mod(rv$h),
                                                              na.rm = TRUE))
          paramsmagnmaximumsa <-
            params$magnmaximumsa[indxs]
          paramsmagnmaximumsf <-
            params$magnmaximumsf[indxs]
        }
        if (params$normalizedMagPlotAmplitude) {
          paramsmagnmaximumsa <-
            params$magnmaximumsa / max(Mod(rv$h),
                                                 na.rm = TRUE)
        }
        else {
          paramsmagnmaximumsa <-
            params$magnmaximumsa / params$edit_gain
        }
      }
    }
    else {
      if (params$logarithmicMagPlotAmplitude) {
        if (params$FFTshifted) {
          paramsmagnmaximumsa <- 20 * log10(x[, 1])
      } else {
          paramsmagnmaximumsa <- 20 * log10(x[, 1])
        }
        if (!(params$normalizedMagPlotAmplitude)) {
          indxs <- which(params$magnmaximumsa < (20L *log10(max(Mod(rv$h), na.rm = TRUE
                                                             )) - params$minimumLogFreqDisplayed))
          paramsmagnmaximumsa <-
            params$magnmaximumsa[indxs]
          paramsmagnmaximumsf <-
            params$magnmaximumsf[indxs]
        }
        if (params$normalizedMagPlotAmplitude) {
          paramsmagnmaximumsa <- params$magnmaximumsa -
            20 * log10(max(Mod(rv$h), na.rm = TRUE))
        }
        else {
          paramsmagnmaximumsa <- params$magnmaximumsa -
            20 * log10(1/ params$edit_gain)
        }
      }
      else {
        if (params$FFTshifted) {
          paramsmagnmaximumsa <- x[, 1]
        }
        else {
          paramsmagnmaximumsa <- x[, 1]
        }
        if (!(params$normalizedMagPlotAmplitude)) {
          indxs <- which(params$magnmaximumsa < max(Mod(rv$h),
                                                              na.rm = TRUE))
          paramsmagnmaximumsa <-
            params$magnmaximumsa[indxs]
          paramsmagnmaximumsf <-
            params$magnmaximumsf[indxs]
        }
        if (params$normalizedMagPlotAmplitude) {
          paramsmagnmaximumsa <-
            params$magnmaximumsa / max(Mod(rv$h),
                                                 na.rm = TRUE)
        }
        else {
          paramsmagnmaximumsa <-
            params$magnmaximumsa / params$edit_gain
        }
      }
    }
    # points(
    #   paramsmagnmaximumsf,
    #   paramsmagnmaximumsa,
    #   type = "b",
    #   lty = "dotted",
    #   lwd = params$LineWidth * 2/3,
    #   pch = 24L,
    #   cex = 2.0,
    #   bg = "maroon",
    #   col = params$grcolor
    # )
  }
  if (!pracma::isempty(xmins)) {
    if (!(is.vector(xmins))) {
      xmins <- xmins[order(xmins[, 2]),]
    }
    if (is.vector(xmins)) {
      xmins[1] <- -(xmins[1])
    }
    else {
      xmins[, 1] <- -(xmins[, 1])
    }
    if (!(is.vector(xmins))) {
  } else {
    }
    if (is.vector(xmins)) {
      if (params$FFTshifted) {
        paramsmagnminimumsf <- (rv$f[xmins[2]] / pi)
      }
      else {
        if (params$twosidedFFT) {
          paramsmagnminimumsf <- rv$f[xmins[2]] / (2 * pi)
        }
        else {
          paramsmagnminimumsf <- rv$f[xmins[2]] / (pi)
        }
      }
    }
    else {
      if (params$FFTshifted) {
        paramsmagnminimumsf <- (rv$f[xmins[, 2]] / pi)
      }
      else {
        if (params$twosidedFFT) {
          paramsmagnminimumsf <- rv$f[xmins[, 2]] / (2 * pi)
        }
        else {
          paramsmagnminimumsf <- rv$f[xmins[, 2]] / (pi)
        }
      }
    }
    if (is.vector(xmins)) {
      if (params$logarithmicMagPlotAmplitude) {
        if (params$FFTshifted) {
          paramsmagnminimumsa <- 20 * log10(xmins[1])
        }
        else {
          paramsmagnminimumsa <- 20 * log10(xmins[1])
        }
        if (params$normalizedMagPlotAmplitude) {
          paramsmagnminimumsa <- params$magnminimumsa -
            20 * log10(max(Mod(rv$h), na.rm = TRUE))
        }
      }
      else {
        if (params$FFTshifted) {
          paramsmagnminimumsa <- xmins[1]
        }
        else {
          paramsmagnminimumsa <- xmins[1]
        }
        if (params$normalizedMagPlotAmplitude) {
          paramsmagnminimumsa <-
            params$magnminimumsa / max(Mod(rv$h),
                                                 na.rm = TRUE)
        }
        else {
          paramsmagnminimumsa <-
            params$magnminimumsa / params$edit_gain
        }
      }
    }
    else {
      if (params$logarithmicMagPlotAmplitude) {
        if (params$FFTshifted) {
          paramsmagnminimumsa <- 20 * log10(xmins[, 1])
        }
        else {
          paramsmagnminimumsa <- 20 * log10(xmins[, 1])
        }
        if (params$normalizedMagPlotAmplitude) {
          paramsmagnminimumsa <- params$magnminimumsa -
            20 * log10(max(Mod(rv$h), na.rm = TRUE))
        }
      }
      else {
        if (params$FFTshifted) {
          paramsmagnminimumsa <- xmins[, 1]
        }
        else {
          paramsmagnminimumsa <- xmins[, 1]
        }
        if (params$normalizedMagPlotAmplitude) {
          paramsmagnminimumsa <-
            params$magnminimumsa / max(Mod(rv$h),
                                                 na.rm = TRUE)
        }
        else {
          paramsmagnminimumsa <-
            params$magnminimumsa / params$edit_gain
        }
      }
    }
    # points(
    #   paramsmagnminimumsf,
    #   paramsmagnminimumsa,
    #   lty = "dotted",
    #   lwd = params$LineWidth,
    #   pch = 25L,
    #   bg = params$BackgroundColor,
    #   col = "maroon"
    # )
  }
}
# end axes_mag replicated
\```

***

\```{r, fig.cap="Stop-Band/ Pass-Band Plots"}
par(mgp = c(2.5, 1, 0))
par(mfrow = c(1, 2))
plot(
 if (params$FFTshifted) {
   (rv$f / pi - params$samplingfreq / 2)
 }
 else {
   if (params$twosidedFFT) {
     rv$f / (2 * pi)
   }
   else {
     rv$f / (pi)
   }
 },
 if (params$logarithmicMagPlotAmplitude) {
   (if (params$FFTshifted) {
     20 * log10(Mod(pracma::fftshift(rv$h)))
   }
   else {
     20 * log10(Mod(rv$h))
   }) - (if (params$normalizedMagPlotAmplitude) {
     20 * log10(max(Mod(rv$h), na.rm = TRUE))
   }
   else {
     20 * log10(1/ params$edit_gain)
   })
 }
 else {
   (if (params$FFTshifted) {
     Mod(pracma::fftshift(rv$h))
   }
   else {
     Mod(rv$h)
   }) / (if (params$normalizedMagPlotAmplitude) {
     max(Mod(rv$h), na.rm = TRUE)
   }
   else {
     1/ params$edit_gain
   })
 },
 xlim = params$zoomlimXpassband,
 ylim = params$zoomlimYpassband,
 type = "l",
 col = params$ForegroundColor,
 bg = params$BackgroundColor,
 lwd = params$LineWidth,
 xlab = if (params$freqaxisunits ==
            "zero2one") {
   expression("Normalized Frequency (" %*% pi ~ ~ "[rads/Sample])")
 }
 else if (params$freqaxisunits == "zero2pi") {
   expression(omega ~ ~ "[rads]")
 }
 else if (params$freqaxisunits == "zero22pi") {
   expression(omega ~ ~ "[rads]")
 }
 else if (params$freqaxisunits == "zero2half") {
   expression(omega / {
     2 * pi
   } ~ ~  ~  ~ "[rads]")
 }
 else if (params$freqaxisunits == "zero2piby2") {
   expression(2L * omega ~ ~  ~  ~ "[rads]")
 }
 else if (params$freqaxisunits == "zero2fs") {
   if (params$twosidedFFT)
     expression(-f[s] %->% ~
                  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
   else
     expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
 }
 else if (params$freqaxisunits == "zero2fsby2") {
   if ((!params$twosidedFFT) &&
       (!params$FFTshifted) && (!params$logarithmicFreqAxis)) {
     expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
   }
   else {
     expression(0L %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
   }
 }
 else if (params$freqaxisunits == "zero2fmax") {
   expression(0L %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
 }
 else if (params$freqaxisunits == "zero2fmaxby2") {
   if (params$twosidedFFT)
     expression(-f[max] /
                  2L %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
   else
     expression(0L %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
 },
 ylab = if ((params$freqaxisunits == "zero2one") ||
            (params$freqaxisunits ==
             "zero2pi") ||
            (params$freqaxisunits == "zero22pi") ||
            (params$freqaxisunits == "zero2half") ||
            (params$freqaxisunits ==
             "zero2piby2")) {
   if (params$normalizedMagPlotAmplitude) {
     if (params$logarithmicMagPlotAmplitude) {
       expression(paste(group("|", bold(H(
         e ^ {
           1i * omega
         }
       )), "|"), " / max [dB]"))
     }
     else {
       expression(paste(group("|", bold(H(
         e ^ {
           1i * omega
         }
       )), "|")), " / max")
     }
   }
   else {
     if (params$logarithmicMagPlotAmplitude) {
       expression(paste(group("|", bold(H(
         e ^ {
           1i * omega
         }
       )), "|"), "  [dB]"))
     }
     else {
       expression(group("|", bold(H(e ^ {
         1i * omega
       })), "|"))
     }
   }
 }
 else {
   if (params$logarithmicMagPlotAmplitude) {
     expression(paste(group("|", bold(H(
       e ^ {
         j ~ ~ 2 * pi * f
       }
     )), "|"), "[dB]"))
   }
   else {
     expression(group("|", bold(H(e ^ {
       j ~ ~ 2 * pi * f
     })), "|"))
   }
 },
 main = "Pass-Band"
)
if (params$checkboxRAY)
 abline(
   v = params$slider1,
   lwd = params$LineWidth,
   col = "magenta",
   lty = "dashed"
 )
grid(col = params$grcolor)
abline(h = 0L, col = "black")
abline(v = 0L, col = "black")
if (params$secondaryaxis) {
 if ((params$freqaxisunits == "zero2pi") || (params$freqaxisunits ==
                                            "zero22pi") ||
     (params$freqaxisunits == "zero2piby2")) {
   axis(
     side = 3L,
     at = pi * c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
     labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
     tick = TRUE,
     pos = par("usr")[4],
     outer = FALSE,
     col = params$grcolor,
     cex.axis = 0.8,
     mgp = c(-2.5,-1L, 0L),
     xlab = "pi-based"
   )
 }
 if ((params$freqaxisunits == "zero2one") ||
     (params$freqaxisunits == "zero2half")
 ) {
   axis(
     side = 3L,
     at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
     labels = expression(-6*pi,-11*pi/2,       -5 * pi,       -9 * pi / 2,       -4 * pi,       -7 * pi / 2,-3 * pi,       -5 * pi / 2,       -2 * pi,       -3 * pi / 2,       -pi,       -pi / 2,-pi / 3,       -pi / 5,       pi / 5,       pi / 3,       pi / 2,       pi,       3 * pi / 2,       2 * pi,       5 * pi /         2L,       3 * pi,       7 * pi / 2,       4 * pi,       9 * pi / 2,       5 * pi,       11 * pi /         2L,       6 * pi     ),
     tick = TRUE,
     pos = par("usr")[4],
     outer = FALSE,
     col = params$grcolor,
     cex.axis = 0.8,
     mgp = c(-2.5,-1L, 0L)
   )
 }
 if ((params$freqaxisunits == "zero2fs") ||
     (params$freqaxisunits == "zero2fmax") ||
     (params$freqaxisunits == "zero2fmaxby2")
 ) {
   axis(
     side = 3L,
     at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L
     ) * params$samplingfreq / 2,
     labels = expression(-6*pi,-11*pi/2,       -5 * pi,       -9L *         pi /         2L,       -4 * pi,       -7 * pi / 2,       -3 * pi,       -5 * pi / 2,       -2L *         pi,       -3 * pi / 2,       -pi,       -pi / 2,       -pi / 3,       -pi / 5,       pi / 5,       pi / 3,       pi /         2L,       pi,       3 * pi / 2,       2 * pi,       5 * pi / 2,       3 * pi,       7L *         pi /         2L,       4 * pi,       9 * pi / 2,       5 * pi,       11 * pi / 2,       6L *         pi     ),
     tick = TRUE,
     line = -0.4,
     pos = par("usr")[4],
     outer = FALSE,
     col = params$grcolor,
     cex.axis = 0.8,
     mgp = c(-2.5,-1L, 0L)
   )
 }
}
if (params$showPhaseOnMagPlot) {
 lines(
   if (params$FFTshifted) {
     (rv$f / pi - params$samplingfreq / 2)
   }
   else {
     if (params$twosidedFFT) {
       rv$f / (2 * pi)
     }
     else {
       rv$f / (pi)
     }
   },
   if (params$FFTshifted) {
     Arg(pracma::fftshift(rv$h))
   }
   else {
     Arg(rv$h)
   },
   col = "grey",
   lwd = params$LineWidth * 2/3,
   lty = "dotted"
 )
 if (params$showLegend)
   legend(
     "topright",
     inset = 0.03,
     c("magn.", "phase"),
     col = c(params$ForegroundColor, "grey"),
     lty = c("solid",
             "dotted"),
     bty = "n",
     cex = 0.8
   )
}
plot(
 if (params$FFTshifted) {
   (rv$f / pi - params$samplingfreq / 2)
 }
 else {
   if (params$twosidedFFT) {
     rv$f / (2 * pi)
   }
   else {
     rv$f / (pi)
   }
 },
 if (params$logarithmicMagPlotAmplitude) {
   (if (params$FFTshifted) {
     20 * log10(Mod(pracma::fftshift(rv$h)))
   }
   else {
     20 * log10(Mod(rv$h))
   }) - (if (params$normalizedMagPlotAmplitude) {
     20 * log10(max(Mod(rv$h), na.rm = TRUE))
   }
   else {
     0L
   })
 }
 else {
   (if (params$FFTshifted) {
     Mod(pracma::fftshift(rv$h))
   }
   else {
     Mod(rv$h)
   }) / (if (params$normalizedMagPlotAmplitude) {
     max(Mod(rv$h), na.rm = TRUE)
   }
   else {
     1/ params$edit_gain
   })
 },
 xlim = params$zoomlimXstopband,
 ylim = params$zoomlimYstopband,
 type = "l",
 col = params$ForegroundColor,
 bg = params$BackgroundColor,
 lwd = params$LineWidth,
 xlab = if (params$freqaxisunits == "zero2one") {
   expression("Normalized Frequency (" %*% pi ~ ~ "[rads/Sample])")
 }
 else if (params$freqaxisunits == "zero2pi") {
   expression(omega ~ ~ "[rads]")
 }
 else if (params$freqaxisunits == "zero22pi") {
   expression(omega ~ ~ "[rads]")
 }
 else if (params$freqaxisunits == "zero2half") {
   expression(omega / {
     2 * pi
   } ~ ~  ~  ~ "[rads]")
 }
 else if (params$freqaxisunits == "zero2piby2") {
   expression(2L * omega ~ ~  ~  ~ "[rads]")
 }
 else if (params$freqaxisunits == "zero2fs") {
   if (params$twosidedFFT)
     expression(-f[s] %->% ~
                  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
   else
     expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
 }
 else if (params$freqaxisunits == "zero2fsby2") {
   if ((!params$twosidedFFT) &&
       (!params$FFTshifted) && (!params$logarithmicFreqAxis)) {
     expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
   }
   else {
     expression(0L %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
   }
 }
 else if (params$freqaxisunits == "zero2fmax") {
   expression(0L %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
 }
 else if (params$freqaxisunits == "zero2fmaxby2") {
   if (params$twosidedFFT)
     expression(-f[max] /
                  2L %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
   else
     expression(0L %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
 },
 ylab = if ((params$freqaxisunits == "zero2one") ||
            (params$freqaxisunits ==
             "zero2pi") ||
            (params$freqaxisunits == "zero22pi") ||
            (params$freqaxisunits == "zero2half") ||
            (params$freqaxisunits ==
             "zero2piby2")) {
   if (params$normalizedMagPlotAmplitude) {
     if (params$logarithmicMagPlotAmplitude) {
       expression(paste(group("|", bold(H(
         e ^ {
           1i * omega
         }
       )), "|"), " / max [dB]"))
     }
     else {
       expression(paste(group("|", bold(H(
         e ^ {
           1i * omega
         }
       )), "|")), " / max")
     }
   }
   else {
     if (params$logarithmicMagPlotAmplitude) {
       expression(paste(group("|", bold(H(
         e ^ {
           1i * omega
         }
       )), "|"), "  [dB]"))
     }
     else {
       expression(group("|", bold(H(e ^ {
         1i * omega
       })), "|"))
     }
   }
 }
 else {
   if (params$logarithmicMagPlotAmplitude) {
     expression(paste(group("|", bold(H(
       e ^ {
         j ~ ~ 2 * pi * f
       }
     )), "|"), "[dB]"))
   }
   else {
     expression(group("|", bold(H(e ^ {
       j ~ ~ 2 * pi * f
     })), "|"))
   }
 },
 main = "Stop-Band"
)
if (params$checkboxRAY)
 abline(
   v = params$slider1,
   lwd = params$LineWidth,
   col = "magenta",
   lty = "dashed"
 )
grid(col = params$grcolor)
abline(h = 0L, col = "black")
abline(v = 0L, col = "black")
if (params$secondaryaxis) {
 if ((params$freqaxisunits == "zero2pi") || 
     (params$freqaxisunits == "zero22pi") ||
     (params$freqaxisunits == "zero2piby2")) {
   axis(
     side = 3L,
     at = pi * c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
     labels = expression(-6*pi,-11*pi/2,       -5 * pi,       -9 * pi / 2,       -4 * pi,       -7 * pi / 2,-3 * pi,       -5 * pi / 2,       -2 * pi,       -3 * pi / 2,       -pi,       -pi / 2,-pi / 3,       -pi / 5,       pi / 5,       pi / 3,       pi / 2,       pi,       3 * pi / 2,       2 * pi,       5 * pi /         2L,       3 * pi,       7 * pi / 2,       4 * pi,       9 * pi / 2,       5 * pi,       11 * pi /         2L,       6 * pi     ),
     tick = TRUE,
     pos = par("usr")[4],
     outer = FALSE,
     col = params$grcolor,
     cex.axis = 0.8,
     mgp = c(-2.5,-1L,0L),
     xlab = "pi-based"
   )
 }
 if ((params$freqaxisunits == "zero2one") ||
     (params$freqaxisunits == "zero2half")) {
   axis(
     side = 3L,
     at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
     labels = expression(-6*pi,-11*pi/2,       -5 * pi,       -9 * pi / 2,       -4 * pi,       -7 * pi / 2,-3 * pi,       -5 * pi / 2,       -2 * pi,       -3 * pi / 2,       -pi,       -pi / 2,-pi / 3,       -pi / 5,       pi / 5,       pi / 3,       pi / 2,       pi,       3 * pi / 2,       2 * pi,       5 * pi /         2L,       3 * pi,       7 * pi / 2,       4 * pi,       9 * pi / 2,       5 * pi,       11 * pi /         2L,       6 * pi     ),
     tick = TRUE,
     pos = par("usr")[4],
     outer = FALSE,
     col = params$grcolor,
     cex.axis = 0.8,
     mgp = c(-2.5,-1L,0L)
   )
 }
 if ((params$freqaxisunits == "zero2fs") ||
     (params$freqaxisunits == "zero2fmax") ||
     (params$freqaxisunits == "zero2fmaxby2")) {
   axis(
     side = 3L,
     at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L
     ) * params$samplingfreq / 2,
     labels = expression(-6*pi,-11*pi/2,       -5 * pi,       -9L *         pi /         2L,       -4 * pi,       -7 * pi / 2,       -3 * pi,       -5 * pi / 2,       -2L *         pi,       -3 * pi / 2,       -pi,       -pi / 2,       -pi / 3,       -pi / 5,       pi / 5,       pi / 3,       pi /         2L,       pi,       3 * pi / 2,       2 * pi,       5 * pi / 2,       3 * pi,       7L *         pi /         2L,       4 * pi,       9 * pi / 2,       5 * pi,       11 * pi / 2,       6L *         pi     ),
     tick = TRUE,
     line = -0.4,
     pos = par("usr")[4],
     outer = FALSE,
     col = params$grcolor,
     cex.axis = 0.8,
     mgp = c(-2.5,-1L, 0L)
   )
 }
}
if (params$showPhaseOnMagPlot) {
 lines(
   if (params$FFTshifted) {
     (rv$f / pi - params$samplingfreq / 2)
   }
   else {
     if (params$twosidedFFT) {
       rv$f / (2 * pi)
     }
     else {
       rv$f / (pi)
     }
   },
   if (params$FFTshifted) {
     Arg(pracma::fftshift(rv$h))
   }
   else {
     Arg(rv$h)
   },
   col = "grey",
   lwd = params$LineWidth * 2/3,
   lty = "dotted"
 )
 if (params$showLegend)
   legend(
     "topright",
     inset = 0.03,
     c("magn.", "phase"),
     col = c(params$ForegroundColor, "grey"),
     lty = c("solid","dotted"),
     bty = "n",
     cex = 0.8
   )
}
par(mfrow = c(1,1))
\```

***

\```{r bCoefficients}
knitr::kable(params$handlesb, caption = "b Coefficients (Mov.-avg. MA)",
            format.args = list(width =45))
\```

\```{r aCoefficients}
knitr::kable(params$handlesa, caption = "a Coefficients (Auto-regr. AR)",
            format.args = list(width =45))
\```

\```{r MinimumValues}
knitr::kable(cbind(minfreqs=params$magnminimumsf,minamplitudes=params$magnminimumsa), 
      caption = "Minimum Values",
      format.args = list(width =25))
\```

\```{r MaximumValues}
knitr::kable(cbind(maxfreqs=params$magnmaximumsf,maxamplitudes=params$magnmaximumsa), 
      caption = "Maximum Values",
      format.args = list(width =25))
\```

\```{r AllParams}
print(params)
\```

***

\```{r figurea1screenshot, echo=FALSE, eval=FALSE, fig.cap="Sample Screen-shot of web-application", fig.align="center", out.width="6in"}
library(webshot)
tempScreenshot <- file.path(tempdir(), "ScreenShotPeZdemoR.png")
list.files(path = params$appwd)
webshot::appshot(app = file.path(params$appwd), file = tempScreenshot, 
    vwidth = 1366, delay = 90L)
\```


***

# Conclusion

Last updated on `r Sys.Date()`.

Session information:

```{r Session Info, comment=NA, background=NA, echo=FALSE}
library(devtools)
devtools::session_info()
citation()
license()
\```
'
          )
        )

        try(file.copy(tempReport, "report.Rmd", overwrite=TRUE), silent=TRUE) # try and make a local-copy to view later
        
        params <-
          list( # pass the parameters for TEMPLATE report.Rmd ----
            n = input$slider,
            inputcommonFilters = input$commonFilters,
            handlesb = handlesb(),
            handlesa = handlesa(),
            handleshn = handleshn(),
            zeroloc = handles$zeroloc,
            poleloc = handles$poleloc,
            normalizedMagPlotAmplitude = input$normalizedMagPlotAmplitude,
            ForegroundColor = input$ForegroundColor,
            LineWidth = input$LineWidth,
            FFTshifted = input$FFTshifted,
            unwrapPhase = input$unwrapPhase,
            twosidedFFT = input$twosidedFFT,
            samplingfreq = input$samplingfreq,
            showPhaseOnMagPlot = input$showPhaseOnMagPlot,
            grcolor = input$grcolor,
            freqaxisunits = input$freqaxisunits,
            secondaryaxis = input$secondaryaxis,
            logarithmicMagPlotAmplitude = input$logarithmicMagPlotAmplitude,
            BackgroundColor = input$BackgroundColor,
            edit_gain = input$edit_gain,
            logarithmicFreqAxis = input$logarithmicFreqAxis,
            checkboxRAY = input$checkboxRAY,
            slider1 = input$slider1,
            showLegend = input$showLegend,
            showMaxMinsOnMagPlot = input$showMaxMinsOnMagPlot,
            magnminimumsa = handles$magnminimumsa,
            magnminimumsf = handles$magnminimumsf,
            magnmaximumsa = handles$magnmaximumsa,
            magnmaximumsf = handles$magnmaximumsf,
            zoomlimXpassband = input$zoomlimXpassband,
            zoomlimYpassband = input$zoomlimYpassband,
            zoomlimXstopband = input$zoomlimXstopband,
            zoomlimYstopband = input$zoomlimYstopband,
            minimumLogFreqDisplayed = input$minimumLogFreqDisplayed,
            includeSourceCode = ((input$reportFormat == "html") || input$reportFormat == "slidy"),
            appwd = getwd()
          )
        rmarkdown::render(
          input = tempReport,
          output_format = "all",
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )

  # output$hover_info - cursor-coordinates, pzplot ----
  output$hover_info <- renderUI({
    Myhover <- input$pzplot_hover
    Mydf <-
      as.data.frame(cbind(xReal = c(
        Re(handles$poleloc), Re(handles$zeroloc)
      ),
      yImag = c(
        Im(handles$poleloc), Im(handles$zeroloc)
      )), row.names = c(paste0("p",
                               1L:length(
                                 handles$poleloc
                               )), paste0("z", 1L:length(
                                 handles$zeroloc
                               ))))
    point <- nearPoints(
      df = Mydf,
      coordinfo = input$pzplot_hover,
      xvar = "xReal",
      yvar = "yImag",
      threshold = 7L,
      maxpoints = 1L,
      addDist = TRUE
    )
    if (!(handles$inDragMode) && (NROW(point) == 0L))
      return(NULL)
    left_pct <-
      (Myhover$x - Myhover$domain$left) / (Myhover$domain$right -
                                             Myhover$domain$left)
    top_pct <-
      (Myhover$domain$top - Myhover$y) / (Myhover$domain$top -
                                            Myhover$domain$bottom)
    left_px <-
      Myhover$range$left + left_pct * (Myhover$range$right -
                                         Myhover$range$left)
    top_px <- Myhover$range$top + top_pct * (Myhover$range$bottom -
                                               Myhover$range$top)
    style <-
      paste(
        "position:absolute;",
        "z-index:100;",
        "background-color: rgba(245, 245, 245, 0.85);",
        "left:",
        paste0(left_px + 2, "px;"),
        "top:",
        paste0(top_px +
                 2 - 50, "px;")
      ) # http://www.77dev.com/2016/03/custom-interactive-csshtml-tooltips.html
    wellPanel(style = style, class = "well-sm", HTML(
      paste(
        "<b>",
        rownames(point),
        "</b>: [",
        "<b>",
        round(point$xReal, 2L),
        "</b>",
        ", ",
        "<b>",
        round(point$yImag, 2L),
        "</b>]"
      )
    ))
  })
  
  # output$hover_info2 - cursor-coordinates, magnitude-plot ----
  output$hover_info2 <- renderUI({
    req(handles$magnmaximumsf, handles$magnminimumsa)
    Myhover <- input$magplot_hover
    if ((length(handles$magnmaximumsa) > 0L) ||
        (length(handles$magnminimumsa) >
         0L)) {
      Mydf <- as.data.frame(cbind(
        frq = c(handles$magnmaximumsf,
                handles$magnminimumsf),
        magn = c(handles$magnmaximumsa,
                 handles$magnminimumsa)
      ), row.names = c(paste0(
        "s", 1L:length(handles$magnmaximumsf)
      ),
      paste0(
        "m", 1L:length(handles$magnminimumsf)
      )))
      point <-
        nearPoints(
          df = Mydf,
          coordinfo = input$magplot_hover,
          xvar = "frq",
          yvar = "magn",
          threshold = 7L,
          maxpoints = 1L,
          addDist = TRUE
        )
      if (NROW(point) == 0L)
        return(NULL)
      left_pct <- (if (input$logarithmicFreqAxis) {
        input$minimumLogFreqDisplayed * log10(Myhover$x)
      }
      else {
        Myhover$x
      } - Myhover$domain$left) / (Myhover$domain$right - Myhover$domain$left)
      top_pct <-
        (Myhover$domain$top - Myhover$y) / (Myhover$domain$top -
                                              Myhover$domain$bottom)
      left_px <-
        Myhover$range$left + left_pct * (Myhover$range$right -
                                           Myhover$range$left)
      top_px <-
        Myhover$range$top + top_pct * (Myhover$range$bottom -
                                         Myhover$range$top)
      style <-
        paste(
          "position:absolute;",
          "z-index:100;",
          "background-color: rgba(245, 245, 245, 0.85);",
          "left:",
          paste0(left_px + 2, "px;"),
          "top:",
          paste0(top_px + 2, "px;")
        )
       # http://www.77dev.com/2016/03/custom-interactive-csshtml-tooltips.html
      wellPanel(style = style, class = "well-sm", HTML(
        paste(
          "<b>",
          rownames(point),
          "</b>: [",
          "<b>",
          round(point$frq, 2L),
          "</b>",
          ", ",
          "<b>",
          round(point$magn, 2L),
          "</b>]"
        )
      ))
    }
  })
  
  # output$downloadCoordsExport ----
  output$downloadCoordsExport <-
    downloadHandler(
      filename = function() {
        paste0(input$filenameExport,
               if (input$IncludeTimeStampExport) {
                 paste0("-", Sys.Date())
               },
               ".",
               input$textbinaryformatExport)
      },
      content = function(file1) {
        if (input$textbinaryformatExport == "csv") {
          write(
            c(
              if (input$coordsheaderExport) {
                input$headerlineExport
              },
              "#polelocs",
              poleloc = handles$poleloc,
              "#zerolocs",
              zeroloc = handles$zeroloc
            ),
            file = file1,
            ncolumns = 1L,
            append = FALSE,
            sep = " "
          )
          # close(file1)
        }
        else if (input$textbinaryformatExport == "bin") {
          # cat(file=stderr(),"L6602 input$filenameExport:",input$filenameExport,".\n")
          zz <- file(paste0(input$filenameExport, ".bin"), open = "wb")
          writeBin(c(
            poleloc = handles$poleloc,
            0L,
            zeroloc = handles$zeroloc
          ),
          con = zz)
          close(zz)
          zz <- file(paste0(input$filenameExport, ".bin"), open = "rb")
          str(zz)
          print(readBin(
            con = zz,
            what = complex(),
            n = 20L # (maximal) number of records to be read
          ))
          close(zz)
        }
        else if (input$textbinaryformatExport == "RData") {
          # cat(file=stderr(),"L6620 input$filenameExport:",input$filenameExport,".\n")
          mypolescopy <- handles$poleloc
          myzeroscopy <- handles$zeroloc
          save(myzeroscopy, mypolescopy, file = "rpolezerolocs.RData")
          load(file = paste0(input$filenameExport, ".RData"),verbose = TRUE)
        }
        else if (input$textbinaryformatExport == "mat") {
          # cat(file=stderr(),"L6627 input$filenameExport:",input$filenameExport,".\n")
          mybcopy <- isolate(handlesb())
          myacopy <- isolate(handlesa())
          R.matlab::writeMat(
            paste0(input$filenameExport, ".mat"),
            Num = mybcopy, # i.e. Numerator
            Den = myacopy, # i.e. Denominator
            verbose = TRUE
          )
        }
        else if (input$textbinaryformatExport == "yml") {
          # cat(file=stderr(),"L6638 input$filenameExport:",input$filenameExport,".\n")
          MyYamlString <-
            yaml::as.yaml(list(
              mypolescopy = as.character(handles$poleloc),
              myzeroscopy = as.character(handles$zeroloc)
            ))
          write(MyYamlString, file = paste0(input$filenameExport, ".yml"))
        }
      }
    )
  randomVals <-
    eventReactive(eventExpr = input$pb_showgph, valueExpr = {
      runif(5L)
    })
  plotInput <- function() {
    hist(randomVals())
  }
  output$plotshowgph <- renderPlot({
    hist(randomVals())
  })
  
  # output$downloadShowgphPlot --- -
  output$downloadShowgphPlot <-
    downloadHandler(
      filename = "PeZplot.png",
      content = function(file) {
        png(
          file,
          width = 480,
          height = 480,
          units = "px",
          pointsize = 12L,
          bg = "white"
        )
        plotInput()
        dev.off()
      }
    )
  
  # observeEvent unitymax_gain ----
  observeEvent(eventExpr = input$unitymax_gain,
               handlerExpr = {
                 filtb <- handlesb()
                 filta <- handlesa()
                 filtb[Re(filtb) > 1e+12] <- 1e+12
                 filtb[Re(filtb) < -1e+12] <- -1e+12
                 filta[Re(filta) > 1e+12] <- 1e+12
                 filta[Re(filta) < -1e+12] <- -1e+12
                 filtb[Im(filtb) > 1e+12] <- Re(filtb) + 1i * 1e+12
                 filtb[Im(filtb) < -1e+12] <- Re(filtb) - 1i * 1e+12
                 filta[Im(filta) > 1e+12] <- Re(filtb) + 1i * 1e+12
                 filta[Im(filta) < -1e+12] <- Re(filtb) - 1i * 1e+12
                 rv <-
                   signal::freqz(
                     filtb,
                     filta,
                     region = "whole",
                     n = 2 ^ 20,
                     Fs = 2 *
                       pi * input$samplingfreq / 2
                   )
                 updateNumericInput(session, inputId = "edit_gain", value = 1/ max(Re(rv$h),na.rm=TRUE))
                 updateCheckboxInput(session, inputId = "normalizedMagPlotAmplitude",
                                     value = TRUE)
               })
  
  # handlesa reactive ----
  handlesa <- reactive({
    updateSelectInput(session,
                      inputId = "listbox_a",
                      choices = round(pracma::Poly(handles$poleloc), 6L))
    shinyjs::disable(id = "listbox_a")
    rawcalc <- pracma::Poly(handles$poleloc)
    rawcalc
  })

  # handlesb reactive ----
  handlesb <- reactive({
    updateSelectInput(session,
                      inputId = "listbox_b",
                      choices = round(pracma::Poly(handles$zeroloc), 6L))
    shinyjs::disable(id = "listbox_b")
    rawcalc <- pracma::Poly(handles$zeroloc)
    rawcalc
  })
  
  # handleshz reactive ----
  handleshz <- reactive({
    N <- 512
    pt <- input$maxLengthImpulseResponse
    pracma::fftshift((fft(c(
      input$edit_gain * handlesb(), rep(0L, N -
                                          length(input$edit_gain * handlesb()))
    ))) / c(fft(c(
      handlesa(),
      rep(0L, N - length(handlesa()))
    ))))
  })
  
  hnimag <- reactive({
    pt <- input$maxLengthImpulseResponse
    if ((length(handlesb()) >= 2L) && (length(handlesa()) >= 2L)) {
      temphnimag <-
        signal::filter(c(handlesb()[1], Im(handlesb()[2L:length(handlesb())])),
                       c(handlesa()[1], Im(handlesa()[2L:length(handlesa())])),
                       c(1L, rep(0L, times = (pt - 1L)))) # impulse
    }
    else if (length(handlesa()) >= 2L) {
      temphnimag <-
        signal::filter(handlesb(), c(handlesa()[1], Im(handlesa()[2L:length(handlesa())])),
                       c(1L, rep(0L, times = (pt - 1L)))) # impulse
    }
    else {
      temphnimag <- try(signal::filter(handlesb(), 
                                       handlesa(), 
                                       c(1L, rep(0L, 
                                                 times = (pt - 1L)
                                                 )
                                         )
                                       ),silent=TRUE)
    }
    if (length(temphnimag) < 2L) {
      temphnimag <- temphnimag * 0L
    }
    else if (max(abs(temphnimag[2L:length(temphnimag)]),na.rm=TRUE) > 0L) {
      temphnimag <- temphnimag * 0L
      temphnimag[2] <- (0 + 1i) * Im(handles$poleloc[1])
    }
    temphnimag
  })
  
  hnimagu <- reactive({
    pt <- input$maxLengthImpulseResponse
    if ((length(handlesb()) >= 2L) && (length(handlesa()) >= 2L)) {
      temphnimag <-
        signal::filter(c(handlesb()[1], Im(handlesb()[2L:length(handlesb())])),
                       c(handlesa()[1], Im(handlesa()[2L:length(handlesa())])),
                       c(1L, rep(1L, times = (pt - 1L)))) # step
    }
    else if (length(handlesa()) >= 2L) {
      temphnimag <-
        signal::filter(handlesb(), c(handlesa()[1], Im(handlesa()[2L:length(handlesa())])),
                       c(1L, rep(1L, times = (pt - 1L)))) # step
    }
    else {
      temphnimag <- try(signal::filter(handlesb(), 
                                       handlesa(), 
                                       c(1L, 
                                         rep(0L, 
                                             times = (pt - 1L)
                                             )
                                         )
                                       ),silent=TRUE)
    }
    if (length(temphnimag) < 2L) {
      temphnimag <- temphnimag * 0L
    }
    else if (max(abs(temphnimag[2L:length(temphnimag)]),na.rm=TRUE) > 0L) {
      temphnimag <- temphnimag * 0L
      temphnimag[2] <- (0 + 1i) * Im(handles$poleloc[1])
    }
    temphnimag
  })
  
  # handleshn reactive ----
  handleshn <- reactive({
    pt <- input$maxLengthImpulseResponse
    # signal::impz(
    #   filt = input$edit_gain * handlesb(),
    #   a = handlesa(),
    #   n = pt - 1L
    # )
    signal::filter( # real-parts only - imaginary-parts are "discarded in coercion"
      filt = input$edit_gain * handlesb(),
      a = handlesa(),
      x = c(1L, rep(0L, times = (pt - 1L))) # impulse
    )
  })
  
  # handleshnu reactive ----
  handleshnu <- reactive({
    pt <- input$maxLengthImpulseResponse
    # signal::impz(
    #   filt = input$edit_gain * handlesb(),
    #   a = handlesa(),
    #   n = pt - 1L
    # )
    signal::filter(
      filt = input$edit_gain * handlesb(),
      a = handlesa(),
      x = c(1, rep(1, times = (pt - 1L))) # unit-step
    )
  })
  
  # observeEvent zoomenable ----
  observeEvent(eventExpr = input$tb_zoomenable,
               handlerExpr = {
                 shinyjs::toggle(condition = input$tb_zoomenable, selector = "#ResultsPanel li a[data-value=Zoom]")
                 if (input$tb_zoomenable) {
                   updateTabsetPanel(session, inputId = "ResultsPanel", selected = "Zoom")
                 }
                 else {
                   updateTabsetPanel(session, inputId = "ResultsPanel", selected = "Magnitude")
                 }
               })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # observeEvent pzplot_brush ----
  observeEvent(eventExpr = input$pzplot_brush, handlerExpr = {
    if (handles$inDragMode)
      return(NULL)
    brush <- input$pzplot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    }
    else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  # output$axes_pzplot ----
  output$axes_pzplot <-
    renderPlot(width = "auto", height = "auto", {
      req(input$slider1)
      nc <- 100L
      cc <- 1L * exp((0L + (0 + 1i)) * 2 * pi * c(0L:(nc - 1L)) / (nc - 1L)) # unit-circle
      usr1 <- par("usr")[1]
      usr2 <- par("usr")[2]
      usr3 <- par("usr")[3]
      usr4 <- par("usr")[4]
      xlims <-
        c(max(-5L, 1.05 * min(
          Re(handles$poleloc),
          Re(handles$zeroloc),
          input$zoomlimX[1],
          na.rm = TRUE
        ), na.rm = TRUE),
        min(5L, 1.05 *
              max(
                Re(handles$poleloc),
                Re(handles$zeroloc),
                input$zoomlimX[2],
                na.rm = TRUE
              ), na.rm = TRUE))
      ylims <-
        c(max(-5L, 1.05 * min(
          Im(handles$poleloc),
          Im(handles$zeroloc),
          input$zoomlimY[1],
          na.rm = TRUE
        ), na.rm = TRUE),
        min(5L, 1.05 *
              max(
                Im(handles$poleloc),
                Im(handles$zeroloc),
                input$zoomlimY[2],
                na.rm = TRUE
              ), na.rm = TRUE))
      plot(
        1L * cc, # unit-circle
        type = if (input$showUnitCircle) {
          "l"
        }
        else {
          "n"
        },
        lty = if (input$showUnitCircle) {
          "solid"
        }
        else {
          "dotted"
        },
        lwd = input$LineWidth,
        col = if (input$showUnitCircle) {
          input$ForegroundColor
        }
        else {
          "transparent"
        },
        bg = input$BackgroundColor,
        xlim = xlims,
        ylim = ylims,
        main = "Pole-Zero Plot",
        xlab = "Re(z)",
        ylab = "Im(z) * j",
        las = 0L,
        asp = 1L
      )
      if (input$checkboxRAY) {
        # must determine quadrant http://www.intmath.com/trigonometric-functions/5-signs-of-trigonometric-functions.php
        switch(3L + sign(sin(
          input$slider1 * pi / (input$samplingfreq / 2)
        )) +
          sign(cos(
            input$slider1 * pi / (input$samplingfreq / 2)
          )),
        {
          rayX <- c(0L,-10L)
          rayY <-
            c(0L,-10L * tan(input$slider1 * pi / (input$samplingfreq / 2)))
        },
        if (abs(sin(
          input$slider1 * pi / (input$samplingfreq / 2)
        )) <=
        2 * eps) {
          rayX <- c(0L,-10L)
          rayY <- c(0L, 0L)
        } else {
          rayX <- c(0L, 0L)
          rayY <- c(0L,-10L)
        },
        if (sin(input$slider1 * pi / (input$samplingfreq / 2)) > 0L) {
          rayX <- c(0L,-10L)
          rayY <-
            c(0L,-10L * tan(input$slider1 * pi / (input$samplingfreq / 2)))
        } else {
          rayX <- c(0L, 10L)
          rayY <-
            c(0L, 10L * tan(input$slider1 * pi / (input$samplingfreq / 2)))
        },
        if (abs(sin(
          input$slider1 * pi / (input$samplingfreq / 2)
        )) <=
        2 * eps) {
          rayX <- c(0L, 10L)
          rayY <- c(0L, 0L)
        } else {
          rayX <- c(0L, 0L)
          rayY <- c(0L, 10L)
        },
        {
          rayX <- c(0L, 10L)
          rayY <-
            c(0L, 10L * tan(input$slider1 * pi / (input$samplingfreq / 2)))
        })
        if (abs(rayX[1]) < eps)
          rayX[1] <- 0L
        if (abs(rayX[2]) < eps)
          rayX[2] <- 0L
        if (abs(rayY[1]) < eps)
          rayY[1] <- 0L
        if (abs(rayY[2]) < eps)
          rayY[2] <- 0L
        lines(rayX, rayY, lwd = input$LineWidth, col = "magenta")
      }
      if (handles$inDragMode) {
        box(
          which = "plot",
          col = "magenta",
          lwd = input$LineWidth,
          lty = "dotted"
        )
        box(
          which = "inner",
          col = "magenta",
          lwd = input$LineWidth,
          lty = "dotted"
        )
        grid(col = "magenta")
        abline(h = 0L, col = "magenta")
        abline(v = 0L, col = "magenta")
      }
      if (max(Mod(handles$poleloc),na.rm=TRUE) > (1L + 1e-04)) {
        box(
          which = "plot",
          col = MyColourForUnstableSystem,
          lwd = 3L *
            input$LineWidth,
          lty = "dashed"
        )
        box(
          which = "inner",
          col = MyColourForUnstableSystem,
          lwd = 3L *
            input$LineWidth,
          lty = "dotted"
        )
        symbols(
          0L,
          0L,
          circles = 0.99,
          inches = FALSE,
          bg = MyColourForUnstableSystem,
          add = TRUE
        )
        grid(col = MyColourForUnstableSystem)
        abline(h = 0L, col = MyColourForUnstableSystem)
        abline(v = 0L, col = MyColourForUnstableSystem)
      }
      rmin <- 0.2
      rstep <- rmin
      rmax <- 10L - rstep
      for (r in (seq(rmin, rmax, by = rstep))) {
        lines(r * cc, lty = "dotted", col = if (input$polargrid) {input$grcolor} else {"transparent"})
      }
      lines(
        1L * cc, # unit-circle
        lty = if (input$showUnitCircle) {
          "solid"
        }
        else {
          "dotted"
        },
        lwd = input$LineWidth,
        col = if (input$showUnitCircle) {
          input$ForegroundColor
        }
        else {
          "transparent"
        }
      )
      ell <- seq(rmin, 10L, by = 1/ nc)
      tmin <- pi / 12
      for (t in (seq(tmin, 2 * pi, by = tmin))) {
        r <- cos(t) * ell + (0L + (0 + 1i)) * sin(t) * ell
        lines(Re(r), Im(r), lty = "dotted", col = if (input$polargrid) {input$grcolor} else {"transparent"})
        text(
          cos(t),
          sin(t),
          if (input$degreesgrid) {
            round(t * 180 / pi)
          }
          else {
            if (t <= pi / 3) {
              substitute(paste(frac(pi, MYVALUE)), list(MYVALUE = round(pi / t, 1)))
            }
            else if ((t > pi / 3) && (t < pi)) {
              substitute(paste(frac(MYVALUE1 * pi, MYVALUE2)),
                         list(
                           MYVALUE1 = sub("^1$", # "^1L$",
                                          "", as.character(unlist(
                                            strsplit(attr(
                                              MASS::fractions(t / pi,
                                                              max.denominator = 13), "fracs"
                                            ), "/")
                                          )[1])),
                           MYVALUE2 = as.character(unlist(strsplit(
                             attr(MASS::fractions(t / pi,
                                                  max.denominator = 13), "fracs"), "/"
                           ))[2])
                         ))
            }
            else if ((t >= pi) && (t <= 2 * pi)) {
              substitute(paste(MYVALUE, pi), list(MYVALUE = round((t / pi), 2L)))
            }
          },
          col = if (input$polargrid) {input$grcolor} else {"transparent"},
          adj = c(0.5 - 0.73 * cos(t), 0.5 - 0.73 * sin(t))
        )
      }
      tmin <- pi / 8
      for (t in (seq(tmin, 2 * pi, by = tmin))) {
        r <- cos(t) * ell + (0L + (0 + 1i)) * sin(t) * ell
        lines(Re(r), Im(r), lty = "dotted", col = if (input$polargrid) {input$grcolor} else {"transparent"})
        text(
          cos(t),
          sin(t),
          if (input$degreesgrid) {
            round(t * 180 / pi)
          }
          else {
            if (t <= pi / 3) {
              substitute(paste(frac(pi, MYVALUE)), list(MYVALUE = round(pi/t, 1L)))
            }
            else if ((t > pi / 3) && (t < pi)) {
              substitute(paste(frac(MYVALUE1 * pi, MYVALUE2)),
                         list(
                           MYVALUE1 = sub("^1$", # "^1L$",
                                          "", as.character(unlist(
                                            strsplit(attr(
                                              MASS::fractions(t / pi,
                                                              max.denominator = 13L), "fracs"
                                            ), "/")
                                          )[1])),
                           MYVALUE2 = as.character(unlist(strsplit(
                             attr(MASS::fractions(t / pi,
                                                  max.denominator = 13L), "fracs"), "/"
                           ))[2])
                         ))
            }
            else if ((t >= pi) && (t <= 2 * pi)) {
              substitute(paste(MYVALUE * pi), list(MYVALUE = round((t / pi), 2L)))
            }
          },
          col = if (input$polargrid) {input$grcolor} else {"transparent"},
          adj = c(0.46 - 0.73 * cos(t), 0.5 - 0.73 * sin(t))
        )
      }
      grid(col = input$grcolor)
      abline(h = 0, col = "black")
      abline(v = 0, col = "black")
      
      if (input$rootlocusgrid) { # http://octave.1599824.n4.nabble.com/new-function-zgrid-m-td1646613.html
        # ported from MATLAB after Tarmigan Casebolt, 2007
        texton <- 1L
        
        zeta <- seq(0.1,1, by=0.1)
        # wn_a <- pracma::linspace(0L,2*pi,n=100L)
        wn_a <- seq(0,2, length.out=100L) * pi
        
        # plot the lines for z
        rv <- pracma::meshgrid(zeta,wn_a)
        zz <- rv$X
        wwn <- rv$Y
        
        s <- -zz * wwn + 1i*wwn * sqrt(1L - zz^2)
        z <- exp(s)
        
        # Do this so that we don't wrap around.
        # z(imag(z)<0L) <- real(z(imag(z)<0L));
        # We do this ugly `for` loop, so that we don't make an ugly dark-line
        # on the real-axis.
        for (q in 1L:dim(z)[2]) {
          y <- z[,q]
          y[Im(y)<0] <- Re(y[which( Im(y)<0L ,1L)])
          z[,q] <- y
        }
        
        # par(pty="s")
        matlines(
          Re(z), Im(z),
          # type="l",
          col=input$grcolor,
          lwd=input$LineWidth/2
          # ,xlim=c(-1L,1L)
          # ,ylim=c(-1L,1L)
          ,lty = "dotted"
        )
        matlines(
          Re(Conj(z)), Im(Conj(z)),
          col=input$grcolor,
          lty = "dotted",
          lwd=input$LineWidth/2
        )
        
        if (texton > 0L) {
          # put in text labels for zeta
          for (q in 1L:(dim(z)[2])) {
            loc <- z[pracma::ceil(dim(z)[1]/4),q]
            if (texton == 2L) {
              text(Re(loc),Im(loc),
                   # paste0('zeta = ',pracma::num2str(zeta[q]))
                   substitute(zeta== ZETAQ, 
                              list(ZETAQ=zeta[q]))
                   , cex=1.0
                   , col=input$grcolor
                   , adj = c(0.46 - 0.65 * cos(Arg(loc)), 0.5 - 0.65 * sin(Arg(loc)))
                   )
            } else {
              text(Re(loc),Im(loc),
                   # paste0(pracma::num2str(zeta[q]))
                   zeta[q]
                   , cex=1.0
                   , col=input$grcolor
                   , adj = c(0.46 - 0.65 * cos(Arg(loc)), 0.5 - 0.65 * sin(Arg(loc)))
                   )
            }
          }
        }
        
        # zeta_a <- pracma::linspace(0L,1L, n=50L)
        zeta_a <- seq(0L,1L, length.out=50L)
        wn <- seq(0,1, by=0.1) * pi
        
        # plot the lines for wn.
        rv <- pracma::meshgrid(zeta_a,wn)
        zz <- rv$X
        wwn <- rv$Y
        
        s <- -zz*wwn + 1i*wwn*sqrt(1L-zz^2)
        z <- exp(s)
        
        matlines(
          Re(t(z)), Im(t(z)),
          col=input$grcolor,
          lty = "dotted",
          lwd=input$LineWidth/2
        )
        matlines(
          Re(Conj(t(z))),Im(Conj(t(z))),
          col=input$grcolor,
          lty = "dotted",
          lwd=input$LineWidth/2
        )
        
        if (texton) {
          # put in the text labels for wn
          for (q in 1L:(dim(z)[1])) {
            if (texton == 2L) {
              text(Re(z[q,1]),0.03+Im(z[q,1]),
                   # paste0('wn = ',10L,pracma::num2str(wn[q]/pi),'*pi/T')
                   substitute(w[n]== WNQbyPi*scriptstyle(over(pi,T)),
                              list(WNQbyPi=wn[q]/pi)
                   )
                   , cex=1.0
                   , col=input$grcolor
                   , adj = c(0.46 - 0.65 * cos(Arg(z[q,1])), 0.5 - 0.65 * sin(Arg(z[q,1])))
                   )
            } else {
              text(Re(z[q,1]),Im(z[q,1]),
                   # paste0(pracma::num2str(wn[q]/pi),'*pi/T')
                   substitute(WNQbyPi*scriptstyle(over(pi,T)),
                              list(WNQbyPi=wn[q]/pi)
                   )
                   , cex=1.0
                   , col=input$grcolor
                   , adj = c(0.46 - 0.65 * cos(Arg(z[q,1])), 0.5 - 0.65 * sin(Arg(z[q,1])))
                   )
            }
          }
        }
        grid(col = input$grcolor)
        abline(h = 0L, col = "black")
        abline(v = 0L, col = input$grcolor)

      }
      
      points(
        Re(handles$zeroloc),
        Im(handles$zeroloc),
        pch = 21L,
        cex = input$LineWidth,
        lwd = input$LineWidth,
        col = input$ForegroundColorZeros,
        bg = input$BackgroundColor
      )
      points(
        Re(handles$poleloc),
        Im(handles$poleloc),
        pch = 4L,
        cex = input$LineWidth,
        lwd = input$LineWidth,
        col = input$ForegroundColorPoles
      )
      nCentralBalancePoints <-
        length(handles$poleloc) - length(handles$zeroloc)
      if ((length(handles$poleloc) == 1L) &&
          (handles$poleloc[1] == 0L) # isTRUE(all.equal( #
          ) {
        nCentralBalancePoints <- nCentralBalancePoints - 1L
      }
      else if ((length(handles$zeroloc) == 1L) &&
               (handles$zeroloc[1] == 0L) # isTRUE(all.equal( #
               ) {
        nCentralBalancePoints <- nCentralBalancePoints + 1L
      }
      if (nCentralBalancePoints > 0L) {
        points(
          rep(0L, nCentralBalancePoints),
          rep(0L, nCentralBalancePoints),
          pch = 21L,
          cex = input$LineWidth / 2,
          col = input$ForegroundColorZeros,
          bg = input$BackgroundColor,
          lwd = input$LineWidth
        )
        if (nCentralBalancePoints > 1L) {
          text(
            0L,
            0L,
            labels = nCentralBalancePoints,
            pos = 1L,
            offset = 0.8,
            cex = input$LineWidth / 2,
            col = input$ForegroundColorZeros,
            bg = input$BackgroundColor
          )
        }
      }
      else if (nCentralBalancePoints < 0L) {
        points(
          rep(0L,-nCentralBalancePoints),
          rep(0L,-nCentralBalancePoints),
          pch = 4L,
          cex = input$LineWidth / 2,
          col = input$ForegroundColorPoles,
          lwd = input$LineWidth
        )
        if (-nCentralBalancePoints > 1L) {
          text(
            0L,
            0L,
            labels = -nCentralBalancePoints,
            pos = 3L,
            offset = 0.7,
            cex = input$LineWidth / 2,
            col = input$ForegroundColorPoles
          )
        }
      }
      MyTableZeros <-
        matrix(data = 0L,
               nrow = length(handles$zeroloc),
               ncol = 2L)
      MyTableZeros[, 1] <- handles$zeroloc
      for (i in 1L:NROW(MyTableZeros)) {
        MyTableZeros[i, 2] <- sum(MyTableZeros[, 1] == MyTableZeros[i, 1]) # isTRUE(all.equal( #
      }
      MyTableZeros <- MyTableZeros[(Re(MyTableZeros[, 2]) > 1L),]
      MyTableZeros <- unique(MyTableZeros)
      if (NROW(MyTableZeros) > 0L) {
        text(
          Re(MyTableZeros[, 1]),
          Im(MyTableZeros[, 1]),
          labels = Re(MyTableZeros[,
                                   2]),
          pos = 1L,
          offset = 0.8,
          cex = input$LineWidth / 2,
          col = input$ForegroundColorZeros,
          bg = input$BackgroundColor
        )
      }
      MyTablePoles <-
        matrix(data = 0L,
               nrow = length(handles$poleloc),
               ncol = 2L)
      MyTablePoles[, 1] <- handles$poleloc
      for (i in 1L:NROW(MyTablePoles)) {
        MyTablePoles[i, 2] <- sum(MyTablePoles[, 1] == MyTablePoles[i, 1]) # isTRUE(all.equal( #
      }
      MyTablePoles <- MyTablePoles[(Re(MyTablePoles[, 2]) > 1L),]
      MyTablePoles <- unique(MyTablePoles)
      if (NROW(MyTablePoles) > 0L) {
        text(
          Re(MyTablePoles[, 1]),
          Im(MyTablePoles[, 1]),
          labels = Re(MyTablePoles[,
                                   2]),
          pos = 3L,
          offset = 0.7,
          cex = input$LineWidth / 2,
          col = input$ForegroundColorPoles
        )
      }
      MyFarAwayZeros <-
        matrix(data = 0L,
               nrow = length(handles$zeroloc),
               ncol = 2L)
      usr1 <- par("usr")[1]
      usr2 <- par("usr")[2]
      usr3 <- par("usr")[3]
      usr4 <- par("usr")[4]
      if ((min(Re(handles$zeroloc), na.rm = TRUE) < usr1) |
          (max(Re(handles$zeroloc),
               na.rm = TRUE) > usr2) |
          (min(Im(handles$zeroloc), na.rm = TRUE) <
           usr3) |
          (max(Im(handles$zeroloc), na.rm = TRUE) > usr4)) {
        MyFarAwayZeros[, 1] <- handles$zeroloc
        MyFarAwayZeros <- MyFarAwayZeros[((Re(MyFarAwayZeros[, 1]) <
                                             usr1) |
                                            (Re(MyFarAwayZeros[, 1]) > usr2) | (Im(MyFarAwayZeros[,
                                                                                                  1]) < usr3) |
                                            (Im(MyFarAwayZeros[, 1]) > usr4)),]
        if (length(MyFarAwayZeros) == 2L) {
          MyFarAwayZeros <- matrix(data = MyFarAwayZeros,
                                   ncol = 2L,
                                   byrow = TRUE)
        }
        for (i in 1L:NROW(MyFarAwayZeros)) {
          MyFarAwayZeros[i, 2] <-
            sum(MyFarAwayZeros[, 1] == MyFarAwayZeros[i, 1]) # isTRUE(all.equal( #
        }
        MyFarAwayZeros <- unique(MyFarAwayZeros)
        if (NROW(MyFarAwayZeros) > 0L) {
          ReFarAwayZeros <- Re(MyFarAwayZeros[, 1])
          ImFarAwayZeros <- Im(MyFarAwayZeros[, 1])
          for (i in (1L:NROW(MyFarAwayZeros))) {
            usr1 <- par("usr")[1]
            usr2 <- par("usr")[2]
            usr3 <- par("usr")[3]
            usr4 <- par("usr")[4]
            if ((is.infinite(ImFarAwayZeros[i])) &&
                (is.infinite(ReFarAwayZeros[i]))) {
              tantheta <- sign(ImFarAwayZeros[i]) / sign(ReFarAwayZeros[i])
            }
            else {
              tantheta <- ImFarAwayZeros[i] / ReFarAwayZeros[i]
            }
            cottheta <- 1/ tantheta
            Myvec1 <-
              pi + c(
                atan2(usr3, usr1),
                atan2(usr3, 0L),
                atan2(usr3,
                      usr2),
                atan2(0L, usr2),
                atan2(usr4, usr2),
                atan2(usr4,
                      0L),
                atan2(usr4, usr1),
                atan2(0L, usr1)
              )
            mtext(
              side = switch(
                1L + findInterval(
                  x = pi + atan2(ImFarAwayZeros[i],
                                 ReFarAwayZeros[i]),
                  vec = Myvec1,
                  rightmost.closed = FALSE,
                  all.inside = FALSE,
                  left.open = FALSE
                ),
                2L,                1L,                1L,                4L,                4L,                3L,                3L,                2L,                2L
              ),
              text = paste0("(O)", Re(MyFarAwayZeros[i,
                                                     2])),
              at = switch(
                1L + findInterval(
                  x = pi + atan2(ImFarAwayZeros[i],
                                 ReFarAwayZeros[i]),
                  vec = Myvec1,
                  rightmost.closed = FALSE,
                  all.inside = FALSE,
                  left.open = FALSE
                ),
                switch(
                  2L +
                    sign(ImFarAwayZeros[i]),
                  max(usr3, tantheta * usr1,
                      na.rm = TRUE),
                  0L,
                  min(usr4, tantheta * usr1, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ReFarAwayZeros[i]),
                  max(usr1, cottheta *
                        usr3, na.rm = TRUE),
                  0L,
                  min(usr2, cottheta * usr3,
                      na.rm = TRUE)
                ),
                switch(
                  2L + sign(ReFarAwayZeros[i]),
                  max(usr1, cottheta * usr3, na.rm = TRUE),
                  0L,
                  min(usr2,
                      cottheta * usr3, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ImFarAwayZeros[i]),
                  max(usr3, tantheta * usr2, na.rm = TRUE),
                  0L,
                  min(usr4,
                      tantheta * usr2, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ImFarAwayZeros[i]),
                  max(usr3, tantheta * usr2, na.rm = TRUE),
                  0L,
                  min(usr4,
                      tantheta * usr2, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ReFarAwayZeros[i]),
                  max(usr1, cottheta * usr4, na.rm = TRUE),
                  0L,
                  min(usr2,
                      cottheta * usr4, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ReFarAwayZeros[i]),
                  max(usr1, cottheta * usr4, na.rm = TRUE),
                  0L,
                  min(usr2,
                      cottheta * usr4, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ImFarAwayZeros[i]),
                  max(usr3, tantheta * usr1, na.rm = TRUE),
                  0L,
                  min(usr4,
                      tantheta * usr1, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ImFarAwayZeros[i]),
                  max(usr3, tantheta * usr1, na.rm = TRUE),
                  0L,
                  min(usr4,
                      tantheta * usr1, na.rm = TRUE)
                )
              ),
              cex = input$LineWidth *
                2/3,
              col = input$ForegroundColorZeros,
              bg = input$BackgroundColor
            )
          }
        }
      }
      MyFarAwayPoles <-
        matrix(data = 0L,
               nrow = length(handles$poleloc),
               ncol = 2L)
      usr1 <- par("usr")[1]
      usr2 <- par("usr")[2]
      usr3 <- par("usr")[3]
      usr4 <- par("usr")[4]
      if ((min(Re(handles$poleloc), na.rm = TRUE) < usr1) |
          (max(Re(handles$poleloc),
               na.rm = TRUE) > usr2) |
          (min(Im(handles$poleloc), na.rm = TRUE) <
           usr3) |
          (max(Im(handles$poleloc), na.rm = TRUE) > usr4)) {
        isolate({
          MyFarAwayPoles[, 1] <- handles$poleloc
          MyFarAwayPoles <- MyFarAwayPoles[((Re(MyFarAwayPoles[,
                                                               1]) < usr1) |
                                              (Re(MyFarAwayPoles[, 1]) > usr2) | (Im(MyFarAwayPoles[,
                                                                                                    1]) < usr3) |
                                              (Im(MyFarAwayPoles[, 1]) > usr4)),]
          if (length(MyFarAwayPoles) == 2L) {
            MyFarAwayPoles <- matrix(data = MyFarAwayPoles,
                                     ncol = 2L,
                                     byrow = TRUE)
          }
        })
        if (NROW(MyFarAwayPoles) > 0L) {
          for (i in 1L:NROW(MyFarAwayPoles)) {
            isolate({
              MyFarAwayPoles[i, 2] <- sum(MyFarAwayPoles[, 1] ==
                                            MyFarAwayPoles[i, 1]) # isTRUE(all.equal( #
            })
          }
        }
        isolate({
          MyFarAwayPoles <- unique(MyFarAwayPoles)
        })
        if (NROW(MyFarAwayPoles) > 0L) {
          ReFarAwayPoles <- Re(MyFarAwayPoles[, 1])
          ImFarAwayPoles <- Im(MyFarAwayPoles[, 1])
          for (i in (1L:NROW(MyFarAwayPoles))) {
            if ((is.infinite(ImFarAwayPoles[i])) &&
                (is.infinite(ReFarAwayPoles[i]))) {
              tantheta <- sign(ImFarAwayPoles[i]) / sign(ReFarAwayPoles[i])
            }
            else {
              tantheta <- ImFarAwayPoles[i] / ReFarAwayPoles[i]
            }
            cottheta <- 1/ tantheta
            usr1 <- par("usr")[1]
            usr2 <- par("usr")[2]
            usr3 <- par("usr")[3]
            usr4 <- par("usr")[4]
            Myvec1 <-
              pi + c(
                atan2(usr3, usr1),
                atan2(usr3, 0L),
                atan2(usr3,
                      usr2),
                atan2(0L, usr2),
                atan2(usr4, usr2),
                atan2(usr4,
                      0L),
                atan2(usr4, usr1),
                atan2(0L, usr1)
              )
            mtext(
              side = switch(
                1L + findInterval(
                  x = pi + atan2(ImFarAwayPoles[i],
                                 ReFarAwayPoles[i]),
                  vec = Myvec1,
                  rightmost.closed = FALSE,
                  all.inside = FALSE,
                  left.open = FALSE
                ),
                2L,1L,1L,4L,4L,3L,3L,2L,2L
              ),
              text = paste0("(X)", Re(MyFarAwayPoles[i, 2])),
              at = switch(
                1L + findInterval(
                  x = pi + atan2(ImFarAwayPoles[i],
                                 ReFarAwayPoles[i]),
                  vec = Myvec1,
                  rightmost.closed = FALSE,
                  all.inside = FALSE,
                  left.open = FALSE
                ),
                switch(
                  2L +
                    sign(ImFarAwayPoles[i]),
                  max(usr3, tantheta * usr1, na.rm = TRUE),
                  0L,
                  min(usr4, tantheta * usr1, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ReFarAwayPoles[i]),
                  max(usr1, cottheta * usr3, na.rm = TRUE),
                  0L,
                  min(usr2, cottheta * usr3, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ReFarAwayPoles[i]),
                  max(usr1, cottheta * usr3, na.rm = TRUE),
                  0L,
                  min(usr2, cottheta * usr3, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ImFarAwayPoles[i]),
                  max(usr3, tantheta * usr2, na.rm = TRUE),
                  0L,
                  min(usr4, tantheta * usr2, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ImFarAwayPoles[i]),
                  max(usr3, tantheta * usr2, na.rm = TRUE),
                  0L,
                  min(usr4, tantheta * usr2, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ReFarAwayPoles[i]),
                  max(usr1, cottheta * usr4, na.rm = TRUE),
                  0L,
                  min(usr2, cottheta * usr4, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ReFarAwayPoles[i]),
                  max(usr1, cottheta * usr4, na.rm = TRUE),
                  0L,
                  min(usr2, cottheta * usr4, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ImFarAwayPoles[i]),
                  max(usr3, tantheta * usr1, na.rm = TRUE),
                  0L,
                  min(usr4, tantheta * usr1, na.rm = TRUE)
                ),
                switch(
                  2L + sign(ImFarAwayPoles[i]),
                  max(usr3, tantheta * usr1, na.rm = TRUE),
                  0L,
                  min(usr4, tantheta * usr1, na.rm = TRUE)
                )
              ),
              cex = input$LineWidth * 2/3,
              col = input$ForegroundColorPoles
            )
          }
        }
      }
    })
  
  # output$axes_pzplotZoom ----
  output$axes_pzplotZoom <-
    renderPlot(width = "auto", height = "auto",
               {
                 nc <- 100L
                 cc <-
                   1L * exp((0L + (0 + 1i)) * 2 * pi * c(0L:(nc - 1L)) / (nc - 1L)) # unit-circle
                 usr1 <- par("usr")[1]
                 usr2 <- par("usr")[2]
                 usr3 <- par("usr")[3]
                 usr4 <- par("usr")[4]
                 xlims <-
                   c(max(-5L, 1.05 * min(
                     Re(handles$poleloc),
                     Re(handles$zeroloc),
                     input$zoomlimX[1],
                     na.rm = TRUE
                   ), na.rm = TRUE),
                   min(5L,
                       1.05 * max(
                         Re(handles$poleloc),
                         Re(handles$zeroloc),
                         input$zoomlimX[2],
                         na.rm = TRUE
                       ), na.rm = TRUE))
                 ylims <-
                   c(max(-5L, 1.05 * min(
                     Im(handles$poleloc),
                     Im(handles$zeroloc),
                     input$zoomlimY[1],
                     na.rm = TRUE
                   ), na.rm = TRUE),
                   min(5L,
                       1.05 * max(
                         Im(handles$poleloc),
                         Im(handles$zeroloc),
                         input$zoomlimY[2],
                         na.rm = TRUE
                       ), na.rm = TRUE))
                 if (input$tb_zoomenable) {
                   xlims <- ranges$x
                   ylims <- ranges$y
                 }
                 plot(
                   1L * cc, # unit-circle
                   type = if (input$showUnitCircle) {
                     "l"
                   }
                   else {
                     "n"
                   },
                   lty = if (input$showUnitCircle) {
                     "solid"
                   }
                   else {
                     "dotted"
                   },
                   lwd = input$LineWidth,
                   col = if (input$showUnitCircle) {
                     input$ForegroundColor
                   }
                   else {
                     "transparent"
                   },
                   bg = input$BackgroundColor,
                   xlim = xlims,
                   ylim = ylims,
                   main = "Pole-Zero Plot (Select desired-region within the other plot)",
                   xlab = "Re(z)",
                   ylab = "Im(z) * j",
                   las = 0L,
                   asp = 1L
                 )
                 if (input$checkboxRAY) {
                   switch(3L + sign(sin(
                     input$slider1 * pi / (input$samplingfreq / 2)
                   )) +
                     sign(cos(
                       input$slider1 * pi / (input$samplingfreq / 2)
                     )),
                   {
                     rayX <- c(0L,-10L)
                     rayY <-
                       c(0L,-10L * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   },
                   if (abs(sin(
                     input$slider1 * pi / (input$samplingfreq / 2)
                   )) <=
                   2 * eps) {
                     rayX <- c(0L,-10L)
                     rayY <- c(0L, 0L)
                   } else {
                     rayX <- c(0L, 0L)
                     rayY <-
                       c(0L,-10L)
                   },
                   if (sin(input$slider1 * pi / (input$samplingfreq / 2)) >
                       0L) {
                     rayX <- c(0L,-10L)
                     rayY <-
                       c(0L,-10L * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   } else {
                     rayX <- c(0L, 10L)
                     rayY <-
                       c(0L, 10L * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   },
                   if (abs(sin(
                     input$slider1 * pi / (input$samplingfreq / 2)
                   )) <=
                   2 * eps) {
                     rayX <- c(0L, 10L)
                     rayY <- c(0L, 0L)
                   } else {
                     rayX <- c(0L, 0L)
                     rayY <- c(0L, 10L)
                   },
                   {
                     rayX <- c(0L, 10L)
                     rayY <-
                       c(0L, 10L * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   })
                   if (abs(rayX[1]) < eps)
                     rayX[1] <- 0L
                   if (abs(rayX[2]) < eps)
                     rayX[2] <- 0L
                   if (abs(rayY[1]) < eps)
                     rayY[1] <- 0L
                   if (abs(rayY[2]) < eps)
                     rayY[2] <- 0L
                   lines(rayX, rayY, lwd = input$LineWidth, col = "magenta")
                 }
                 if (max(Mod(handles$poleloc),na.rm=TRUE) > (1L + 1e-04)) {
                   box(
                     which = "plot",
                     col = MyColourForUnstableSystem,
                     lwd = 3L *
                       input$LineWidth,
                     lty = "dashed"
                   )
                   box(
                     which = "inner",
                     col = MyColourForUnstableSystem,
                     lwd = 3L *
                       input$LineWidth,
                     lty = "dotted"
                   )
                   symbols(
                     0L,
                     0L,
                     circles = 0.99,
                     inches = FALSE,
                     bg = MyColourForUnstableSystem,
                     add = TRUE
                   )
                   grid(col = MyColourForUnstableSystem)
                   abline(h = 0L, col = MyColourForUnstableSystem)
                   abline(v = 0L, col = MyColourForUnstableSystem)
                 }
                 rmin <- 0.2
                 rstep <- rmin
                 rmax <- 10L - rstep
                 for (r in (seq(rmin, rmax, by = rstep))) {
                   lines(r * cc, lty = "dotted", col = if (input$polargrid) {input$grcolor} else {"transparent"})
                 }
                 if (input$showUnitCircle) {
                   lines(
                     1L * cc, # unit-circle
                     lty = if (input$showUnitCircle) {
                       "solid"
                     }
                     else {
                       "dotted"
                     },
                     lwd = input$LineWidth,
                     col = if (input$showUnitCircle) {
                       input$ForegroundColor
                     }
                     else {
                       "transparent"
                     }
                   )
                 }
                 ell <-
                   seq(rmin, 10L, by = 1/ nc)
                 tmin <- pi / 12
                 for (t in (seq(tmin, 2 * pi, by = tmin))) {
                   r <- cos(t) * ell + (0L + (0 + 1i)) * sin(t) * ell
                   lines(Re(r), Im(r), lty = "dotted", col = if (input$polargrid) {input$grcolor} else {"transparent"})
                   text(
                     cos(t),
                     sin(t),
                     if (input$degreesgrid) {
                       round(t * 180 / pi)
                     }
                     else {
                       if (t <= pi / 3) {
                         substitute(paste(frac(pi, MYVALUE)), list(MYVALUE = round(pi / t,
                                                                                   1L)))
                       }
                       else if ((t > pi / 3) &&
                                (t < pi)) {
                         substitute(paste(frac(MYVALUE1 * pi, MYVALUE2)),
                                    list(
                                      MYVALUE1 = sub("^1$", # "^1L$",
                                                     "", as.character(unlist(
                                                       strsplit(attr(
                                                         MASS::fractions(t / pi,
                                                                         max.denominator = 13L), "fracs"
                                                       ), "/")
                                                     )[1])),
                                      MYVALUE2 = as.character(unlist(strsplit(
                                        attr(MASS::fractions(t / pi,
                                                             max.denominator = 13L), "fracs"), "/"
                                      ))[2])
                                    ))
                       }
                       else if ((t >= pi) &&
                                (t <= 2 * pi)) {
                         substitute(paste(MYVALUE, pi), list(MYVALUE = round((t / pi),
                                                                             2L)))
                       }
                     },
                     col = if (input$polargrid) {input$grcolor} else {"transparent"},
                     adj = c(0.5 - 1L * cos(t), 0.5 - 1L * sin(t))
                   )
                 }
                 tmin <- pi / 8
                 for (t in (seq(tmin, 2 * pi, by = tmin))) {
                   r <- cos(t) * ell + (0L + (0 + 1i)) * sin(t) * ell
                   lines(Re(r), Im(r), lty = "dotted", col = if (input$polargrid) {input$grcolor} else {"transparent"})
                   text(
                     cos(t),
                     sin(t),
                     if (input$degreesgrid) {
                       round(t * 180 / pi)
                     }
                     else {
                       if (t <= pi / 3) {
                         substitute(paste(frac(pi, MYVALUE)), list(MYVALUE = round(pi / t,
                                                                                   1L)))
                       }
                       else if ((t > pi / 3) &&
                                (t < pi)) {
                         substitute(paste(frac(MYVALUE1 * pi, MYVALUE2)),
                                    list(
                                      MYVALUE1 = sub("^1$", # "^1L$",
                                                     "", as.character(unlist(
                                                       strsplit(attr(
                                                         MASS::fractions(t / pi,
                                                                         max.denominator = 13L), "fracs"
                                                       ), "/")
                                                     )[1])),
                                      MYVALUE2 = as.character(unlist(strsplit(
                                        attr(MASS::fractions(t / pi,
                                                             max.denominator = 13L), "fracs"), "/"
                                      ))[2])
                                    ))
                       }
                       else if ((t >= pi) &&
                                (t <= 2 * pi)) {
                         substitute(paste(MYVALUE, pi), list(MYVALUE = round((t / pi),
                                                                             2L)))
                       }
                     },
                     col = if (input$polargrid) {input$grcolor} else {"transparent"},
                     adj = c(0.5 - 1.05 * cos(t), 0.5 - 1.05 * sin(t))
                   )
                 }
                 grid(col = if (input$polargrid) {input$grcolor} else {"transparent"})
                 abline(h = 0L, col = "black")
                 abline(v = 0L, col = "black")
                 points(
                   Re(handles$zeroloc),
                   Im(handles$zeroloc),
                   pch = 21L,
                   cex = input$LineWidth,
                   lwd = input$LineWidth,
                   col = input$ForegroundColorZeros,
                   bg = input$BackgroundColor
                 )
                 points(
                   Re(handles$poleloc),
                   Im(handles$poleloc),
                   pch = 4L,
                   cex = input$LineWidth,
                   lwd = input$LineWidth,
                   col = input$ForegroundColorPoles
                 )
                 nCentralBalancePoints <-
                   length(handles$poleloc) - length(handles$zeroloc)
                 if ((length(handles$poleloc) == 1L) &&
                     (handles$poleloc[1] == 0L) # isTRUE(all.equal( #
                     ) {
                   nCentralBalancePoints <- nCentralBalancePoints - 1L
                 }
                 else if ((length(handles$zeroloc) == 1L) &&
                          (handles$zeroloc[1] == 0L) # isTRUE(all.equal( #
                          ) {
                   nCentralBalancePoints <- nCentralBalancePoints + 1L
                 }
                 if (nCentralBalancePoints > 0L) {
                   points(
                     rep(0L, nCentralBalancePoints),
                     rep(0L, nCentralBalancePoints),
                     pch = 21L,
                     cex = input$LineWidth / 2,
                     col = input$ForegroundColorZeros,
                     bg = input$BackgroundColor,
                     lwd = input$LineWidth
                   )
                   if (nCentralBalancePoints > 1L) {
                     text(
                       0L,
                       0L,
                       labels = nCentralBalancePoints,
                       pos = 1L,
                       offset = 0.8,
                       cex = input$LineWidth /
                         2L,
                       col = input$ForegroundColorZeros,
                       bg = input$BackgroundColor
                     )
                   }
                 }
                 else if (nCentralBalancePoints < 0L) {
                   points(
                     rep(0L,-nCentralBalancePoints),
                     rep(0L,-nCentralBalancePoints),
                     pch = 4L,
                     cex = input$LineWidth / 2,
                     col = input$ForegroundColorPoles,
                     lwd = input$LineWidth
                   )
                   if (-nCentralBalancePoints > 1L) {
                     text(
                       0L,
                       0L,
                       labels = -nCentralBalancePoints,
                       pos = 3L,
                       offset = 0.7,
                       cex = input$LineWidth / 2,
                       col = input$ForegroundColorPoles
                     )
                   }
                 }
                 MyTableZeros <-
                   matrix(data = 0L,
                          nrow = length(handles$zeroloc),
                          ncol = 2L)
                 MyTableZeros[, 1] <-
                   handles$zeroloc
                 for (i in 1L:NROW(MyTableZeros)) {
                   MyTableZeros[i, 2] <- sum(MyTableZeros[, 1] == MyTableZeros[i, 1]) # isTRUE(all.equal( #
                 }
                 MyTableZeros <-
                   MyTableZeros[(Re(MyTableZeros[, 2]) > 1L),]
                 MyTableZeros <-
                   unique(MyTableZeros)
                 if (NROW(MyTableZeros) > 0L) {
                   text(
                     Re(MyTableZeros[, 1]),
                     Im(MyTableZeros[, 1]),
                     labels = Re(MyTableZeros[,
                                              2]),
                     pos = 1L,
                     offset = 0.8,
                     cex = input$LineWidth * 2/3,
                     col = input$ForegroundColorZeros,
                     bg = input$BackgroundColor
                   )
                 }
                 MyTablePoles <-
                   matrix(data = 0L,
                          nrow = length(handles$poleloc),
                          ncol = 2L)
                 isolate({
                   MyTablePoles[, 1] <- handles$poleloc
                   for (i in 1L:NROW(MyTablePoles)) {
                     MyTablePoles[i, 2] <- sum(MyTablePoles[, 1] == MyTablePoles[i, 1]) # isTRUE(all.equal( #
                   }
                   MyTablePoles <-
                     MyTablePoles[(Re(MyTablePoles[, 2]) > 1L),]
                   MyTablePoles <-
                     unique(MyTablePoles)
                   if (NROW(MyTablePoles) > 0L) {
                     text(
                       Re(MyTablePoles[, 1]),
                       Im(MyTablePoles[, 1]),
                       labels = Re(MyTablePoles[,
                                                2]),
                       pos = 3L,
                       offset = 0.7,
                       cex = input$LineWidth *
                         2/3,
                       col = input$ForegroundColorPoles
                     )
                   }
                 })
               })
  
  # output$axes_pzplotPolar ----
  output$axes_pzplotPolar <-
    renderPlot(width = "auto", height = "auto",
               {
                 nc <- 100L
                 cc <-
                   1L * exp((0L + (0 + 1i)) * 2 * pi * c(0L:(nc - 1L)) / (nc - 1L)) # unit-circle
                 pracma::polar(
                   Arg(cc),
                   Mod(cc),
                   type = if (input$showUnitCircle) {
                     "l"
                   }
                   else {
                     "n"
                   },
                   lty = if (input$showUnitCircle) {
                     "solid"
                   }
                   else {
                     "dotted"
                   },
                   lwd = input$LineWidth,
                   col = if (input$showUnitCircle) {
                     input$ForegroundColor
                   }
                   else {
                     "transparent"
                   },
                   bg = input$BackgroundColor,
                   main = "Pole-Zero Plot, Polar",
                   xlab = "Re(z)",
                   ylab = "Im(z) * j"
                 )
                 if (input$checkboxRAY) {
                   switch(3L + sign(sin(
                     input$slider1 * pi / (input$samplingfreq / 2)
                   )) +
                     sign(cos(
                       input$slider1 * pi / (input$samplingfreq / 2)
                     )),
                   {
                     rayX <- c(0L,-10L)
                     rayY <-
                       c(0L,-10L * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   },
                   if (abs(sin(
                     input$slider1 * pi / (input$samplingfreq / 2)
                   )) <=
                   2 * eps) {
                     rayX <- c(0L,-10L)
                     rayY <- c(0L, 0L)
                   } else {
                     rayX <- c(0L, 0L)
                     rayY <-
                       c(0L,-10L)
                   },
                   if (sin(input$slider1 * pi / (input$samplingfreq / 2)) >
                       0L) {
                     rayX <- c(0L,-10L)
                     rayY <-
                       c(0L,-10L * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   } else {
                     rayX <- c(0L, 10L)
                     rayY <-
                       c(0L, 10L * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   },
                   if (abs(sin(
                     input$slider1 * pi / (input$samplingfreq / 2)
                   )) <=
                   2 * eps) {
                     rayX <- c(0L, 10L)
                     rayY <- c(0L, 0L)
                   } else {
                     rayX <- c(0L, 0L)
                     rayY <-
                       c(0L, 10L)
                   },
                   {
                     rayX <- c(0L, 10L)
                     rayY <-
                       c(0L, 10L * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   })
                   if (abs(rayX[1]) < eps)
                     rayX[1] <- 0L
                   if (abs(rayX[2]) < eps)
                     rayX[2] <- 0L
                   if (abs(rayY[1]) < eps)
                     rayY[1] <- 0L
                   if (abs(rayY[2]) < eps)
                     rayY[2] <- 0L
                   lines(rayX, rayY, lwd = input$LineWidth, col = "magenta")
                 }
                 pracma::polar(
                   atan2(Im(handles$zeroloc), Re(handles$zeroloc)),
                   sqrt(
                     Re(handles$zeroloc) * Re(handles$zeroloc) + Im(handles$zeroloc) *
                       Im(handles$zeroloc)
                   ),
                   type = "p",
                   lwd = input$LineWidth,
                   pch = 21L,
                   col = input$ForegroundColorZeros,
                   bg = input$BackgroundColor,
                   add = TRUE,
                   cex = input$LineWidth
                 )
                 pracma::polar(
                   atan2(Im(handles$poleloc), Re(handles$poleloc)),
                   sqrt(
                     Re(handles$poleloc) * Re(handles$poleloc) + Im(handles$poleloc) *
                       Im(handles$poleloc)
                   ),
                   type = "p",
                   lwd = input$LineWidth,
                   pch = 4L,
                   col = input$ForegroundColorPoles,
                   add = TRUE,
                   cex = input$LineWidth
                 )
                 axis(side = 1L)
                 axis(side = 2L)
                 if (input$showUnitCircle) {
                   lines(
                     1L * sin(seq(
                       from = 0L,
                       to = 2 * pi,
                       by = 2 * pi / 40L
                     )),
                     1L * cos(seq(
                       from = 0L,
                       to = 2 * pi,
                       by = 2 * pi / 40L
                     )), # unit-circle
                     col = input$ForegroundColor,
                     lty = "solid",
                     lwd = input$LineWidth
                   )
                 }
                 grid(col = if (input$polargrid) {input$grcolor} else {"transparent"})
                 abline(h = 0L, col = "black")
                 abline(v = 0L, col = "black")
                 pracma::polar(
                   atan2(Im(handles$zeroloc), Re(handles$zeroloc)),
                   sqrt(
                     Re(handles$zeroloc) * Re(handles$zeroloc) + Im(handles$zeroloc) *
                       Im(handles$zeroloc)
                   ),
                   type = "p",
                   lwd = input$LineWidth,
                   pch = 21L,
                   col = input$ForegroundColorZeros,
                   bg = input$BackgroundColor,
                   add = TRUE,
                   cex = input$LineWidth
                 )
                 pracma::polar(
                   atan2(Im(handles$poleloc), Re(handles$poleloc)),
                   sqrt(
                     Re(handles$poleloc) * Re(handles$poleloc) + Im(handles$poleloc) *
                       Im(handles$poleloc)
                   ),
                   type = "p",
                   lwd = input$LineWidth,
                   pch = 4L,
                   col = input$ForegroundColorPoles,
                   add = TRUE,
                   cex = input$LineWidth
                 )
               })
  
  # output$axes_beamformer ----
  output$axes_beamformer <-
    renderPlot(width = "auto", height = "auto",
               {
                 filtb <- handlesb()
                 filta <- handlesa()
                 filtb[Re(filtb) > 1e+12] <-
                   1e+12
                 filtb[Re(filtb) < -1e+12] <-
                   -1e+12
                 filta[Re(filta) > 1e+12] <-
                   1e+12
                 filta[Re(filta) < -1e+12] <-
                   -1e+12
                 filtb[Im(filtb) > 1e+12] <-
                   Re(filtb) + 1i * 1e+12
                 filtb[Im(filtb) < -1e+12] <-
                   Re(filtb) - 1i * 1e+12
                 filta[Im(filta) > 1e+12] <-
                   Re(filtb) + 1i * 1e+12
                 filta[Im(filta) < -1e+12] <-
                   Re(filtb) - 1i * 1e+12
                 rv <-
                   signal::freqz(
                     filtb,
                     filta,
                     region = "whole",
                     n = 2 ^ 20,
                     Fs = 2 * pi * input$samplingfreq / 2
                   )
                 radjusted <-
                   60 + 20 * log10(Mod(rv$h)) - 20 * log10(max(Mod(rv$h),
                                                               na.rm = TRUE))
                 radjusted[radjusted < 0] <- 0
                 radjusted[radjusted > 60] <- 60
                 pracma::polar(
                   t = rv$f,
                   r = radjusted,
                   xlim = c(0, 60),
                   ylim = c(0, 60),
                   type = "l",
                   col = input$ForegroundColor,
                   lwd = input$LineWidth,
                   main = "Beamformer-Plot (Spatial-Filtering of Array), Polar",
                   yaxt = "n"
                 )
                 if (input$checkboxRAY) {
                   pracma::polar(
                     c(0, input$slider1 * pi),
                     c(0, 60),
                     add = TRUE,
                     lwd = input$LineWidth,
                     col = "magenta"
                   )
                 }
                 axis(
                   side = 2L,
                   pos = 0L,
                   at = c(0, 10, 20, 30, 40, 50, 60),
                   labels = c(-60,-50,-40,-30,-20,-10, 0),
                   las = 1L
                 )
                 grid(col = if (input$polargrid) {input$grcolor} else {"transparent"})
                 abline(h = 0, col = "black")
                 abline(v = 0, col = "black")
               })
  
  # output$axes_pzplot3D ----
  require(threejs)
  output$axes_pzplot3D <- threejs::renderScatterplotThree({
    withProgress(message = "Calculation in progress", detail = "This may take a while...",
                 {
                   x <- seq(-4.2, 4.2, by = 0.005)
                   y <- seq(-4.2, 4.2, by = 0.005)
                   rv <- pracma::meshgrid(x, y)
                   z <- rv$X + rv$Y * 1i
                   shiny::setProgress(0.2)
                   aPolyValueAtz <- pracma::polyval(handlesa(), z)
                   if (max(abs(aPolyValueAtz),na.rm=TRUE) > 2 * eps) {
                     Hz <- pracma::polyval(handlesb(), z) / aPolyValueAtz # http://stackoverflow.com/questions/21264968/matlab-3d-plot-of-transfer-function-magnitude
                   }
                   else {
                     Hz <- pracma::polyval(handlesb(), z)
                   }
                   shiny::setProgress(0.3)
                   HzReal <- Re(Hz)
                   HzImag <- Im(Hz)
                   HzReal <- pmin(HzReal, 1e+13)
                   HzReal <- pmax(HzReal,-1e+13)
                   HzReal[(abs(HzReal) < 1e-12)] <- 1e-12
                   HzImag <- pmin(HzImag, 1e+13)
                   HzImag <- pmax(HzImag,-1e+13)
                   HzImag[(abs(HzImag) < 1e-12)] <- 1e-12
                   Hz <- HzReal + HzImag * 1i
                   filtb <- handlesb()
                   filta <- handlesa()
                   filtb[Re(filtb) > 1e+12] <- 1e+12
                   filtb[Re(filtb) < -1e+12] <- -1e+12
                   filta[Re(filta) > 1e+12] <- 1e+12
                   filta[Re(filta) < -1e+12] <- -1e+12
                   filtb[Im(filtb) > 1e+12] <- Re(filtb) + 1i * 1e+12
                   filtb[Im(filtb) < -1e+12] <-
                     Re(filtb) - 1i * 1e+12
                   filta[Im(filta) > 1e+12] <- Re(filtb) + 1i * 1e+12
                   filta[Im(filta) < -1e+12] <-
                     Re(filtb) - 1i * 1e+12
                   rv <-
                     signal::freqz(filtb, filta, region = "whole", n = 1024L)
                   xx <- rep(x, times = length(y))
                   yy <- rep(y, each = length(x))
                   zz <- as.vector(Mod(Hz))
                   dim(zz) <- NULL
                   
                   # http://blog.revolutionanalytics.com/2016/02/multivariate_data_with_r.html
                   ra <-
                     ceiling(input$colors3D * (20 * log10(zz) - 20 * log10(min(
                       Mod(Hz),
                       na.last = TRUE
                     ))) / (20 * log10(max(zz,na.rm=TRUE)) - 20 * log10(min(zz,na.rm=TRUE))))
                   col <- topo.colors(input$colors3D)
                   shiny::setProgress(0.4)
                   zz <- 20 * log10(zz)
                   zz[zz == 0] <- NA # isTRUE(all.equal( #
                   x <-
                     threejs::scatterplot3js(
                       x = xx,
                       y = yy,
                       z = zz,
                       size = input$sizes3D,
                       color = col[ra],
                       renderer = input$renderer3D,
                       grid = input$usegrid3D,
                       axis = TRUE,
                       num.ticks = c(input$nticks3D, input$nticks3D,
                                     input$nticks3D),
                       flip.y = TRUE,
                       bg = "#ffffff"
                     )
                   if (input$showUnitCircle) {
                     print(
                       x$points3d(
                         1L * cos(rv$f + pi / 2),
                         1L * sin(rv$f + pi / 2), # unit-circle
                         20 * log10(Mod(rv$h)),
                         color = "red",
                         labels = "freq-response",
                         size = input$sizes3D
                       )
                     )
                   }
                   shiny::setProgress(0.6)
                   if (input$usezerozplane) {
                     x$points3d(
                       seq(-2.5, 2.5, length.out = 1001L),
                       0L,
                       0L,
                       color = "black",
                       labels = "x-axis",
                       size = 0.1
                     )
                     x$points3d(
                       0L,
                       seq(-2.5, 2.5, length.out = 1001L),
                       0L,
                       color = "black",
                       labels = "y-axis",
                       size = 0.1
                     )
                     x$points3d(
                       0L,
                       0L,
                       seq(-2.5, 2.5, length.out = 1001L),
                       color = "black",
                       labels = "z-axis",
                       size = 0.1
                     )
                     x$points3d(
                       seq(-2.5, 2.5, length.out = 20L),
                       seq(-2.5, 2.5, length.out = 20L),
                       0L,
                       color = "black",
                       labels = "grid z=0",
                       size = 0.2
                     )
                     x$points3d(
                       seq(-2.5, 2.5, length.out = input$nticks3D),
                       seq(-2.5, 2.5, length.out = 201L),
                       0L,
                       color = input$grcolor,
                       labels = "grid z=0",
                       size = 0.2
                     )
                   }
                 })
  })
  
  # output$axes_pzplotRGL ----
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  output$axes_pzplotRGL <- rgl::renderRglwidget({
    try(rgl.close())
    withProgress(message = "Calculation in progress", detail = "This may take a while...",
                 {
                   x <- seq(-1.3, 1.3, length.out = 256L)
                   y <- seq(-1.3, 1.3, length.out = 256L)
                   rvmg <- pracma::meshgrid(x, y)
                   z <- t(rvmg$X + rvmg$Y * 1i)
                   shiny::setProgress(0.3)
                   aPolyValueAtz <- pracma::polyval(handlesa(), z)
                   if (max(abs(aPolyValueAtz), na.rm = TRUE) > 2 * eps) {
                     HdB <- pracma::polyval(handlesb(), z) / aPolyValueAtz # http://stackoverflow.com/questions/21264968/matlab-3d-plot-of-transfer-function-magnitude
                   }
                   else {
                     HdB <- pracma::polyval(handlesb(), z)
                   }
                   HdBReal <- Re(HdB)
                   HdBImag <- Im(HdB)
                   HdBReal <- pmin(HdBReal, 1e+13)
                   HdBReal <- pmax(HdBReal,-1e+13)
                   HdBReal[(abs(HdBReal) < 1e-12)] <- 1e-12
                   HdBImag <- pmin(HdBImag, 1e+13)
                   HdBImag <- pmax(HdBImag,-1e+13)
                   HdBImag[(abs(HdBImag) < 1e-12)] <- 1e-12
                   HdB <- HdBReal + HdBImag * 1i
                   HdB <- 20 * log10(Mod(input$edit_gain * HdB))
                   HdB[abs(HdB) <= 20 * log10(1.04)] <- NA
                   if ((input$showUnitCircle) ||
                       (input$show3DSurface)) {
                     filtb <- handlesb()
                     filta <- handlesa()
                     filtb[Re(filtb) > 1e+12] <- 1e+12
                     filtb[Re(filtb) < -1e+12] <- -1e+12
                     filta[Re(filta) > 1e+12] <- 1e+12
                     filta[Re(filta) < -1e+12] <- -1e+12
                     filtb[Im(filtb) > 1e+12] <-
                       Re(filtb) + 1i * 1e+12
                     filtb[Im(filtb) < -1e+12] <-
                       Re(filtb) - 1i * 1e+12
                     filta[Im(filta) > 1e+12] <-
                       Re(filtb) + 1i * 1e+12
                     filta[Im(filta) < -1e+12] <-
                       Re(filtb) - 1i * 1e+12
                     rv <-
                       signal::freqz(filtb, filta, region = "whole", n = 256)
                     rvhReal <- Re(rv$h)
                     rvhImag <- Im(rv$h)
                     rvhReal <- pmin(rvhReal, 1e+06)
                     rvhReal <- pmax(rvhReal,-1e+06)
                     rvhReal[(abs(rvhReal) < 1e-06)] <- 1e-06
                     rvhImag <- pmin(rvhImag, 1e+06)
                     rvhImag <- pmax(rvhImag,-1e+06)
                     rvhImag[(abs(rvhImag) < 1e-06)] <- 1e-06
                     rvh <- rvhReal + rvhImag * 1i
                   }
                   col2 <-
                     topo.colors(length(HdB))[rank(HdB, na.last = TRUE)]
                   shiny::setProgress(0.6)
                   if (input$show3DSurface) {
                     rgl::persp3d(
                       x,
                       y,
                       HdB,
                       col = if (input$show3DColoured) {
                         col2
                       }
                       else {
                         "transparent"
                       },
                       xlab = "Real(z)",
                       ylab = "Imag(z) * j",
                       zlab = "|H(z)| [dB]",
                       zlim = c(max(c(
                         -130L, min(20 * log10(Mod(
                           input$edit_gain *
                             rvh
                         )), na.rm = TRUE) - 10L
                       ), na.rm = TRUE), min(c(
                         130L,
                         max(20 * log10(Mod(
                           input$edit_gain * rvh
                         )), na.rm = TRUE) +
                           10L
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                   }
                   if (input$showLegend)
                     legend3d(
                       "topleft",
                       c("legend"),
                       bty = "n",
                       cex = 0.5,
                       title = "legend"
                     )
                   if (input$showUnitCircle) {
                     rgl::plot3d(
                       1L * cos(rv$f),
                       1L * sin(rv$f), # unit-circle
                       20 * log10(Mod(input$edit_gain * rvh)),
                       type = "h",
                       col = "red",
                       lwd = input$LineWidth,
                       lty = "dashed",
                       add = TRUE,
                       zlim = c(max(c(
                         -130L, min(20L *
                                     log10(Mod(
                                       input$edit_gain * rvh
                                     )), na.rm = TRUE) -
                           10L
                       ), na.rm = TRUE), min(c(
                         130L, max(20 * log10(Mod(
                           input$edit_gain * rvh
                         )), na.rm = TRUE) + 10L
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                     rgl::lines3d(
                       1L * cos(rv$f),
                       1L * sin(rv$f),
                       20 * log10(Mod(input$edit_gain * rvh)),
                       col = "red",
                       lwd = 10L * input$LineWidth,
                       add = TRUE,
                       zlim = c(max(c(
                         -130L, min(20 * log10(Mod(
                           input$edit_gain * rvh
                         )), na.rm = TRUE) - 10L
                       ), na.rm = TRUE), 
                       min(c(
                         130L,
                         max(20 * log10(Mod(
                           input$edit_gain * rvh
                         )), na.rm = TRUE) +
                           10L
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                   }
                   if (input$checkboxRAY) {
                     switch(3L + sign(sin(
                       input$slider1 * pi / (input$samplingfreq / 2)
                     )) +
                       sign(cos(
                         input$slider1 * pi / (input$samplingfreq / 2)
                       )),
                     {
                       rayX <- c(0L, 1.04 * cos(input$slider1 * pi / (input$samplingfreq / 2)))
                       rayY <-
                         c(0L, 1.04 * sin(input$slider1 * pi / (input$samplingfreq / 2)))
                     },
                     if (abs(sin(
                       input$slider1 * pi / (input$samplingfreq / 2)
                     )) <=
                     2 * eps) {
                       rayX <- c(0L, 1.04 * cos(input$slider1 * pi / (input$samplingfreq / 2)))
                       rayY <- c(0L, 0L)
                     } else {
                       rayX <- c(0L, 0L)
                       rayY <-
                         c(0L, 1.04 * sin(input$slider1 * pi / (input$samplingfreq / 2)))
                     },
                     if (sin(input$slider1 * pi / (input$samplingfreq / 2)) > 0L) {
                       rayX <- c(0L, 1.04 * cos(input$slider1 * pi / (input$samplingfreq / 2)))
                       rayY <-
                         c(0L, 1.04 * sin(input$slider1 * pi / (input$samplingfreq / 2)))
                     } else {
                       rayX <- c(0L, 1.04 * cos(input$slider1 * pi / (input$samplingfreq / 2)))
                       rayY <-
                         c(0L, 1.04 * sin(input$slider1 * pi / (input$samplingfreq / 2)))
                     },
                     if (abs(sin(
                       input$slider1 * pi / (input$samplingfreq / 2) )) <= 2 * eps
                     ) {
                       rayX <- c(0L, 1.04 * cos(input$slider1 * pi / (input$samplingfreq / 2)))
                       rayY <- c(0L, 0L)
                     } else {
                       rayX <- c(0L, 0L)
                       rayY <-
                         c(0L, 1.04 * sin(input$slider1 * pi / (input$samplingfreq / 2)))
                     },
                     {
                       rayX <- c(0L, 1.04 * cos(input$slider1 * pi / (input$samplingfreq / 2)))
                       rayY <-
                         c(0L, 1.04 * sin(input$slider1 * pi / (input$samplingfreq / 2)))
                     })
                     if (abs(rayX[1]) < 1e-12)
                       rayX[1] <- 1e-12
                     if (abs(rayX[2]) < 1e-12)
                       rayX[2] <- 1e-12
                     if (abs(rayY[1]) < 1e-12)
                       rayY[1] <- 1e-12
                     if (abs(rayY[2]) < 1e-12)
                       rayY[2] <- 1e-12
                     rgl::lines3d(
                       rayX,
                       rayY,
                       c(0L, max(20 * log10(
                         Mod(input$edit_gain * rvh)
                       ), na.rm = TRUE)),
                       lwd = 10L * input$LineWidth,
                       col = "magenta",
                       add = TRUE,
                       zlim = c(max(c(
                         -130L, min(20L *
                                     log10(Mod(
                                       input$edit_gain * rvh
                                     )), na.rm = TRUE) - 10L
                       ), na.rm = TRUE), min(c(
                         130L, max(20 * log10(Mod(
                           input$edit_gain *
                             rvh
                         )), na.rm = TRUE) + 10L
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                     rgl::lines3d(
                       rayX,
                       rayY,
                       c(0L, max(20 * log10(
                         Mod(input$edit_gain *
                               rvh)
                       ), na.rm = TRUE)),
                       lwd = 10L * input$LineWidth,
                       col = "magenta",
                       add = TRUE,
                       zlim = c(max(c(
                         -130L, min(20L *
                                     log10(Mod(
                                       input$edit_gain * rvh
                                     )), na.rm = TRUE) -
                           10L
                       ), na.rm = TRUE), min(c(
                         130L, max(20 * log10(Mod(
                           input$edit_gain *
                             rvh
                         )), na.rm = TRUE) + 10L
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                     rgl::plot3d(
                       rayX,
                       rayY,
                       c(0L, min(20 * log10(
                         Mod(input$edit_gain *
                               rvh)
                       ))),
                       type = "h",
                       lwd = input$LineWidth,
                       col = "magenta",
                       add = TRUE,
                       zlim = c(max(c(
                         -130L, min(20 * log10(Mod(
                           input$edit_gain *
                             rvh
                         )), na.rm = TRUE) - 10L
                       ), na.rm = TRUE), min(c(
                         130L,
                         max(20 * log10(Mod(
                           input$edit_gain * rvh
                         )), na.rm = TRUE) +
                           10L
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                     rgl::plot3d(
                       rayX,
                       rayY,
                       c(0L, max(20 * log10(
                         Mod(input$edit_gain *
                               rvh)
                       ), na.rm = TRUE)),
                       type = "s",
                       size = 1L,
                       lwd = input$LineWidth,
                       col = "magenta",
                       add = TRUE,
                       zlim = c(max(c(
                         -130L, min(20L *
                                     log10(Mod(
                                       input$edit_gain * rvh
                                     )), na.rm = TRUE) -
                           10L
                       ), na.rm = TRUE), min(c(
                         130L, max(20 * log10(Mod(
                           input$edit_gain *
                             rvh
                         )), na.rm = TRUE) + 10L
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                     rgl::arrow3d(c(rayX[1], rayY[1], min(c(
                       120, max(20L *
                                  log10(Mod(
                                    input$edit_gain * rvh
                                  )), na.rm = TRUE)
                     ),
                     na.rm = TRUE)),
                     c(rayX[2], rayY[2], min(c(
                       120, max(20L *
                                  log10(Mod(
                                    input$edit_gain * rvh
                                  )), na.rm = TRUE)
                     ),
                     na.rm = TRUE)),
                     type = "extrusion",
                     col = "magenta")
                   }
                   rgl::rglwidget()
                 })
  })
  
  # output$downloadRGL - downloadHandler ----
  output$downloadRGL <-
    downloadHandler(
      filename = "RGL.html",
      content = function(file) {
                   x <- seq(-1.3, 1.3, length.out = 256)
                   y <- seq(-1.3, 1.3, length.out = 256)
                   rvmg <- pracma::meshgrid(x, y)
                   z <- t(rvmg$X + rvmg$Y * 1i)
                   shiny::setProgress(0.3)
                   aPolyValueAtz <- pracma::polyval(handlesa(), z)
                   if (max(abs(aPolyValueAtz), na.rm = TRUE) > 2 * eps) {
                     HdB <- pracma::polyval(handlesb(), z) / aPolyValueAtz # http://stackoverflow.com/questions/21264968/matlab-3d-plot-of-transfer-function-magnitude
                   }
                   else {
                     HdB <- pracma::polyval(handlesb(), z)
                   }
                   HdBReal <- Re(HdB)
                   HdBImag <- Im(HdB)
                   HdBReal <- pmin(HdBReal, 1e+13)
                   HdBReal <- pmax(HdBReal,-1e+13)
                   HdBReal[(abs(HdBReal) < 1e-12)] <- 1e-12
                   HdBImag <- pmin(HdBImag, 1e+13)
                   HdBImag <- pmax(HdBImag,-1e+13)
                   HdBImag[(abs(HdBImag) < 1e-12)] <- 1e-12
                   HdB <- HdBReal + HdBImag * 1i
                   HdB <- 20 * log10(Mod(input$edit_gain * HdB))
                   HdB[abs(HdB) <= 20 * log10(1.04)] <- NA
                   if ((input$showUnitCircle) ||
                       (input$show3DSurface)) {
                     filtb <- handlesb()
                     filta <- handlesa()
                     filtb[Re(filtb) > 1e+12] <- 1e+12
                     filtb[Re(filtb) < -1e+12] <- -1e+12
                     filta[Re(filta) > 1e+12] <- 1e+12
                     filta[Re(filta) < -1e+12] <- -1e+12
                     filtb[Im(filtb) > 1e+12] <-
                       Re(filtb) + 1i * 1e+12
                     filtb[Im(filtb) < -1e+12] <-
                       Re(filtb) - 1i * 1e+12
                     filta[Im(filta) > 1e+12] <-
                       Re(filtb) + 1i * 1e+12
                     filta[Im(filta) < -1e+12] <-
                       Re(filtb) - 1i * 1e+12
                     rv <-
                       signal::freqz(filtb, filta, region = "whole", n = 256)
                     rvhReal <- Re(rv$h)
                     rvhImag <- Im(rv$h)
                     rvhReal <- pmin(rvhReal, 1e+06)
                     rvhReal <- pmax(rvhReal,-1e+06)
                     rvhReal[(abs(rvhReal) < 1e-06)] <- 1e-06
                     rvhImag <- pmin(rvhImag, 1e+06)
                     rvhImag <- pmax(rvhImag,-1e+06)
                     rvhImag[(abs(rvhImag) < 1e-06)] <- 1e-06
                     rvh <- rvhReal + rvhImag * 1i
                   }

        # x <- seq(-1.3, 1.3, length.out = 256)
        # y <- seq(-1.3, 1.3, length.out = 256)
        # rv <- pracma::meshgrid(x, y)
        # z <- rv$X + rv$Y * 1i
        # aPolyValueAtz <- pracma::polyval(handlesa(), z)
        # if (max(abs(aPolyValueAtz), na.rm = TRUE) > 2 * eps) {
        #   HdB <- pracma::polyval(handlesb(), z) / aPolyValueAtz
        # }
        # else {
        #   HdB <- pracma::polyval(handlesb(), z)
        # }
        # HdBReal <- Re(HdB)
        # HdBImag <- Im(HdB)
        # HdBReal <- pmin(HdBReal, 1e+13)
        # HdBReal <- pmax(HdBReal,-1e+13)
        # HdBReal[(abs(HdBReal) < 1e-12)] <- 1e-12
        # HdBImag <- pmin(HdBImag, 1e+13)
        # HdBImag <- pmax(HdBImag,-1e+13)
        # HdBImag[(abs(HdBImag) < 1e-12)] <- 1e-12
        # HdB <- HdBReal + HdBImag * 1i
        # HdB <- 20 * log10(Mod(input$edit_gain * HdB))
        # filtb <- handlesb()
        # filta <- handlesa()
        # filtb[Re(filtb) > 1e+12] <- 1e+12
        # filtb[Re(filtb) < -1e+12] <- -1e+12
        # filta[Re(filta) > 1e+12] <- 1e+12
        # filta[Re(filta) < -1e+12] <- -1e+12
        # filtb[Im(filtb) > 1e+12] <- Re(filtb) + 1i * 1e+12
        # filtb[Im(filtb) < -1e+12] <- Re(filtb) - 1i * 1e+12
        # filta[Im(filta) > 1e+12] <- Re(filtb) + 1i * 1e+12
        # filta[Im(filta) < -1e+12] <- Re(filtb) - 1i * 1e+12
        # rv <- signal::freqz(filtb, filta, region = "whole", n = 256)
                   
        col2 <- topo.colors(length(HdB))[rank(HdB, na.last = TRUE)]
        rgl::persp3d(
          x,
          y,
          HdB,
          col = col2,
          ylab = "Real(z)",
          xlab = "Imag(z) * j",
          zlab = "|H(z)| [dB]",
          zlim = c(max(c(
            -130L, min(20 * log10(Mod(
              input$edit_gain * rvh
            )))
          ), na.rm = TRUE), min(c(
            130L, max(20 * log10(Mod(
              input$edit_gain * rvh
            )), na.rm = TRUE)
          ), na.rm = TRUE))
        )
        if (input$showUnitCircle) {
          rgl::plot3d(
            1L * cos(rv$f + pi / 2),
            1L * sin(rv$f + pi / 2), # unit-circle
            20 * log10(Mod(input$edit_gain * rv$h)),
            type = "h",
            col = "red",
            lwd = input$LineWidth,
            lty = "dashed",
            add = TRUE,
            zlim = c(max(c(
              -130L,
              min(20 * log10(Mod(
                input$edit_gain * rvh
              )), na.rm = TRUE) - 10L
            ), na.rm = TRUE), min(c(
              130L, max(20 * log10(Mod(
                input$edit_gain *
                  rvh
              )), na.rm = TRUE) + 10L
            ), na.rm = TRUE)),
            forceClipregion = TRUE
          )
        }
        if (rmarkdown::pandoc_available())
          htmlwidgets::saveWidget(
            rgl::rglwidget(
              width = input$dimension[1],
              height = input$dimension[2]
            ),
            file = file,
            selfcontained = FALSE, # TRUE,
            libdir = NULL,
            background = "white"
          )
      }
    )
  
  # output$axes_mag ----
  output$axes_mag <- renderPlot(width = "auto", height = "auto", {
    filtb <- handlesb()
    filta <- handlesa()
    filtb[Re(filtb) > 1e+12] <- 1e+12
    filtb[Re(filtb) < -1e+12] <- -1e+12
    filta[Re(filta) > 1e+12] <- 1e+12
    filta[Re(filta) < -1e+12] <- -1e+12
    filtb[Im(filtb) > 1e+12] <- Re(filtb) + 1i * 1e+12
    filtb[Im(filtb) < -1e+12] <- Re(filtb) - 1i * 1e+12
    filta[Im(filta) > 1e+12] <- Re(filtb) + 1i * 1e+12
    filta[Im(filta) < -1e+12] <- Re(filtb) - 1i * 1e+12
    rv <-
      signal::freqz(
        filtb,
        filta,
        region = "whole",
        n = 2L ^ 20L,
        Fs = 2L *
          pi * input$samplingfreq / 2
      )
    par(mgp = c(2.5, 1, 0))
    plot(
      if (input$FFTshifted) {
        (rv$f / pi - input$samplingfreq / 2)
      }
      else {
        if (input$twosidedFFT) {
          rv$f / (2 * pi)
        }
        else {
          rv$f / (pi)
        }
      },
      if (input$logarithmicMagPlotAmplitude) {
        (if (input$FFTshifted) {
          20 * log10(Mod(pracma::fftshift(rv$h)))
        }
        else {
          20 * log10(Mod(rv$h))
        }) - (if (input$normalizedMagPlotAmplitude) {
          20 * log10(max(Mod(rv$h), na.rm = TRUE))
        }
        else {
          20 * log10(1/ input$edit_gain)
        })
      }
      else {
        (if (input$FFTshifted) {
          Mod(pracma::fftshift(rv$h))
        }
        else {
          Mod(rv$h)
        }) / (if (input$normalizedMagPlotAmplitude) {
          max(Mod(rv$h), na.rm = TRUE)
        }
        else {
          1/ input$edit_gain
        })
      },
      log = if (input$logarithmicFreqAxis) {
        "x"
      }
      else {
        ""
      },
      xlim = if (input$logarithmicFreqAxis) {
        c(max(c(input$minimumLogFreqDisplayed, 1e-04 * input$samplingfreq / 2),na.rm=TRUE),
          input$samplingfreq / 2)
      }
      else {
        c(if (input$twosidedFFT) {
          -input$samplingfreq / 2
        } else {
          0L
        }, if (input$FFTshifted) {
          input$samplingfreq / 2
        } else {
          if (input$twosidedFFT) {
            input$samplingfreq / 2
          } else {
            2L * input$samplingfreq / 2
          }
        })
      },
      ylim = if (input$logarithmicMagPlotAmplitude) {
        c(max(-130L, min(-20L, 20 * log10(
          min(Mod(rv$h), na.rm = TRUE) / (if (input$normalizedMagPlotAmplitude) {
            max(Mod(rv$h), na.rm = TRUE)
          } else {
            1/ input$edit_gain
          })
        )),na.rm=TRUE), max(0L, min(120, 20 * log10(
          max(Mod(rv$h), na.rm = TRUE) / (if (input$normalizedMagPlotAmplitude) {
            max(Mod(rv$h), na.rm = TRUE)
          } else {
            1/ input$edit_gain
          })
        ), na.rm = TRUE),na.rm=TRUE))
      }
      else {
        c(0L, max(1L, Mod(rv$h) / (if (input$normalizedMagPlotAmplitude) {
          max(Mod(rv$h), na.rm = TRUE)
        } else {
          1/ input$edit_gain
        }), na.rm = TRUE))
      },
      type = "l",
      col = input$ForegroundColor,
      bg = input$BackgroundColor,
      lwd = input$LineWidth,
      xlab = if (input$freqaxisunits == "zero2one") {
        expression("Normalized Frequency (" %*% pi ~ ~ "[rads/Sample])")
      }
      else if (input$freqaxisunits == "zero2pi") {
        expression(omega ~ ~ "[rads]")
      }
      else if (input$freqaxisunits == "zero22pi") {
        expression(omega ~ ~ "[rads]")
      }
      else if (input$freqaxisunits == "zero2half") {
        expression(omega / {
          2 * pi
        } ~ ~  ~  ~ "[rads]")
      }
      else if (input$freqaxisunits == "zero2piby2") {
        expression(2L * omega ~ ~  ~  ~ "[rads]")
      }
      else if (input$freqaxisunits == "zero2fs") {
        if (input$twosidedFFT)
          expression(-f[s] %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
        else
          expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
      }
      else if (input$freqaxisunits == "zero2fsby2") {
        if ((!input$twosidedFFT) &&
            (!input$FFTshifted) && (!input$logarithmicFreqAxis)) {
          expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
        }
        else {
          expression(0L %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
        }
      }
      else if (input$freqaxisunits == "zero2fmax") {
        expression(0L %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
      }
      else if (input$freqaxisunits == "zero2fmaxby2") {
        if (input$twosidedFFT)
          expression(-f[max] / 2 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
        else
          expression(0L %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
      },
      ylab = if ((input$freqaxisunits == "zero2one") ||
                 (input$freqaxisunits == "zero2pi") ||
                 (input$freqaxisunits == "zero22pi") || 
                 (input$freqaxisunits == "zero2half") ||
                 (input$freqaxisunits == "zero2piby2")) {
        if (input$normalizedMagPlotAmplitude) {
          if (input$logarithmicMagPlotAmplitude) {
            expression(paste(group("|", bold(H(
              e ^ {
                1i * omega
              }
            )), "|"), " / max [dB]"))
          }
          else {
            expression(paste(group("|", bold(H(
              e ^ {
                1i * omega
              }
            )), "|")), " / max")
          }
        }
        else {
          if (input$logarithmicMagPlotAmplitude) {
            expression(paste(group("|", bold(H(
              e ^ {
                1i * omega
              }
            )), "|"), "  [dB]"))
          }
          else {
            expression(group("|", bold(H(e ^ {
              1i * omega
            })), "|"))
          }
        }
      }
      else {
        if (input$logarithmicMagPlotAmplitude) {
          expression(paste(group("|", bold(H(
            e ^ {
              j ~ ~ 2 * pi * f
            }
          )), "|"), "[dB]"))
        }
        else {
          expression(group("|", bold(H(e ^ {
            j ~ ~ 2 * pi * f
          })), "|"))
        }
      },
      main = paste0("Magnitude-Reponse", if (input$normalizedMagPlotAmplitude) {
        " (normalized)"
      })
    )
    if (input$checkboxRAY)
      abline(
        v = input$slider1,
        lwd = input$LineWidth,
        col = "magenta",
        lty = "dashed"
      )
    grid(col = input$grcolor)
    abline(h = 0L, col = "black")
    abline(v = 0L, col = "black")
    if (input$secondaryaxis) {
      if ((input$freqaxisunits == "zero2pi") || 
          (input$freqaxisunits == "zero22pi") ||
          (input$freqaxisunits == "zero2piby2")) {
        axis(
          side = 3L,
          at = pi * c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
          labels = expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
          tick = TRUE,
          pos = par("usr")[4],
          outer = FALSE,
          col = input$grcolor,
          cex.axis = 0.8,
          mgp = c(-2.5,-1L,
                  0L),
          xlab = "pi-based"
        )
      }
      if ((input$freqaxisunits == "zero2one") ||
          (input$freqaxisunits == "zero2half")) {
        axis(
          side = 3L,
          at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
          labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
          tick = TRUE,
          pos = par("usr")[4],
          outer = FALSE,
          col = input$grcolor,
          cex.axis = 0.8,
          mgp = c(-2.5,-1L,
                  0L)
        )
      }
      if ((input$freqaxisunits == "zero2fs") ||
          (input$freqaxisunits == "zero2fmax") ||
          (input$freqaxisunits == "zero2fmaxby2")) {
        axis(
          side = 3L,
          at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L
          ) * input$samplingfreq / 2,
labels=expression(-6*pi,-11*pi/2,-5*pi,-9L*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2L*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
          tick = TRUE,
          line = -0.4,
          pos = par("usr")[4],
          outer = FALSE,
          col = input$grcolor,
          cex.axis = 0.8,
          mgp = c(-2.5,-1L, 0L)
        )
      }
    }
    if (input$showPhaseOnMagPlot) {
      lines(
        if (input$FFTshifted) {
          (rv$f / pi - input$samplingfreq / 2)
        }
        else {
          if (input$twosidedFFT) {
            rv$f / (2 * pi)
          }
          else {
            rv$f / (pi)
          }
        },
        if (input$unwrapPhase) {
          if (input$FFTshifted) {
            unWrap(Arg(pracma::fftshift(rv$h)), tol = (pi - eps))
          }
          else {
            unWrap(Arg(rv$h), tol = (pi - eps))
          }
        }
        else {
          if (input$FFTshifted) {
            Arg(pracma::fftshift(rv$h))
          }
          else {
            Arg(rv$h)
          }
        },
        col = input$grcolor,
        lwd = input$LineWidth * 2/3,
        lty = "dotted"
      )
      if (input$showLegend)
        legend(
          "topright",
          inset = 0.03,
          c("magn.", "phase"),
          col = c(input$ForegroundColor,
                  "grey"),
          lty = c("solid", "dotted"),
          bty = "n",
          cex = 0.8
        )
    }
    if (input$showMaxMinsOnMagPlot == TRUE) {
      x <-
        myfindpeaks(
          Mod(rv$h),
          minpeakheight = 1e-12,
          minpeakdistance = (length(rv$h) / 100L) / 2,
          threshold = 0.001,
          npeaks = 100L
        )
      xmins <-
        myfindpeaks(
          -Mod(rv$h),
          minpeakdistance = (length(rv$h) / 100L) / 2,
          threshold = 0.001,
          npeaks = 100L
        )
      if (!pracma::isempty(x)) {
        if (!(is.vector(x))) {
          x <- x[order(x[, 2]),]
        }
        if (is.vector(x)) {
          
        }
        else {
          
        }
        if (is.vector(x)) {
          if (input$FFTshifted) {
            handles$magnmaximumsf <- (rv$f[x[2]] / pi)
          }
          else {
            if (input$twosidedFFT) {
              handles$magnmaximumsf <- rv$f[x[2]] / (2 * pi)
            }
            else {
              handles$magnmaximumsf <- rv$f[x[2]] / (pi)
            }
          }
        }
        else {
          if (input$FFTshifted) {
            handles$magnmaximumsf <- (rv$f[x[, 2]] / pi)
          }
          else {
            if (input$twosidedFFT) {
              handles$magnmaximumsf <- rv$f[x[, 2]] / (2 * pi)
            }
            else {
              handles$magnmaximumsf <- rv$f[x[, 2]] / (pi)
            }
          }
        }
        if (is.vector(x)) {
          if (input$logarithmicMagPlotAmplitude) {
            if (input$FFTshifted) {
              handles$magnmaximumsa <- 20 * log10(x[1])
            }
            else {
              handles$magnmaximumsa <- 20 * log10(x[1])
            }
            if (!(input$normalizedMagPlotAmplitude)) {
              indxs <- which(isolate(handles$magnmaximumsa) < (20L *
                                                                 log10(max(
                                                                   Mod(rv$h), na.rm = TRUE
                                                                 )) - input$minimumLogFreqDisplayed)) # 0.01))
              handles$magnmaximumsa <-
                isolate(handles$magnmaximumsa[indxs])
              handles$magnmaximumsf <-
                isolate(handles$magnmaximumsf[indxs])
            }
            if (input$normalizedMagPlotAmplitude) {
              handles$magnmaximumsa <- isolate(handles$magnmaximumsa) -
                20 * log10(max(Mod(rv$h), na.rm = TRUE))
            }
            else {
              handles$magnmaximumsa <- isolate(handles$magnmaximumsa) -
                20 * log10(1/ input$edit_gain)
            }
          }
          else {
            if (input$FFTshifted) {
              handles$magnmaximumsa <- x[1]
            }
            else {
              handles$magnmaximumsa <- x[1]
            }
            if (!(input$normalizedMagPlotAmplitude)) {
              indxs <- which(isolate(handles$magnmaximumsa) < max(Mod(rv$h),
                                                                  na.rm = TRUE))
              handles$magnmaximumsa <-
                isolate(handles$magnmaximumsa[indxs])
              handles$magnmaximumsf <-
                isolate(handles$magnmaximumsf[indxs])
            }
            if (input$normalizedMagPlotAmplitude) {
              handles$magnmaximumsa <-
                isolate(handles$magnmaximumsa) / max(Mod(rv$h),
                                                     na.rm = TRUE)
            }
            else {
              handles$magnmaximumsa <-
                isolate(handles$magnmaximumsa) / input$edit_gain
            }
          }
        }
        else {
          if (input$logarithmicMagPlotAmplitude) {
            if (input$FFTshifted) {
              handles$magnmaximumsa <- 20 * log10(x[, 1])
            }
            else {
              handles$magnmaximumsa <- 20 * log10(x[, 1])
            }
            if (!(input$normalizedMagPlotAmplitude)) {
              indxs <- which(isolate(handles$magnmaximumsa) < (20L *
                                                                 log10(max(
                                                                   Mod(rv$h), na.rm = TRUE
                                                                 )) - input$minimumLogFreqDisplayed)) # 0.01))
              handles$magnmaximumsa <-
                isolate(handles$magnmaximumsa[indxs])
              handles$magnmaximumsf <-
                isolate(handles$magnmaximumsf[indxs])
            }
            if (input$normalizedMagPlotAmplitude) {
              handles$magnmaximumsa <- isolate(handles$magnmaximumsa) -
                20 * log10(max(Mod(rv$h), na.rm = TRUE))
            }
            else {
              handles$magnmaximumsa <- isolate(handles$magnmaximumsa) -
                20 * log10(1/ input$edit_gain)
            }
          }
          else {
            if (input$FFTshifted) {
              handles$magnmaximumsa <- x[, 1]
            }
            else {
              handles$magnmaximumsa <- x[, 1]
            }
            if (!(input$normalizedMagPlotAmplitude)) {
              indxs <- which(isolate(handles$magnmaximumsa) < max(Mod(rv$h),
                                                                  na.rm = TRUE))
              handles$magnmaximumsa <-
                isolate(handles$magnmaximumsa[indxs])
              handles$magnmaximumsf <-
                isolate(handles$magnmaximumsf[indxs])
            }
            if (input$normalizedMagPlotAmplitude) {
              handles$magnmaximumsa <-
                isolate(handles$magnmaximumsa) / max(Mod(rv$h),
                                                     na.rm = TRUE)
            }
            else {
              handles$magnmaximumsa <-
                isolate(handles$magnmaximumsa) / input$edit_gain
            }
          }
        }
        points(
          handles$magnmaximumsf,
          handles$magnmaximumsa,
          type = "b",
          lty = "dotted",
          lwd = input$LineWidth * 2/3,
          pch = 24L,
          cex = 2.0,
          bg = "maroon",
          col = input$grcolor
        )
      }
      if (!pracma::isempty(xmins)) {
        if (!(is.vector(xmins))) {
          xmins <- xmins[order(xmins[, 2]),]
        }
        if (is.vector(xmins)) {
          xmins[1] <- -(xmins[1])
        }
        else {
          xmins[, 1] <- -(xmins[, 1])
        }
        if (!(is.vector(xmins))) {
          
        }
        else {
          
        }
        if (is.vector(xmins)) {
          if (input$FFTshifted) {
            handles$magnminimumsf <- (rv$f[xmins[2]] / pi)
          }
          else {
            if (input$twosidedFFT) {
              handles$magnminimumsf <- rv$f[xmins[2]] / (2 * pi)
            }
            else {
              handles$magnminimumsf <- rv$f[xmins[2]] / (pi)
            }
          }
        }
        else {
          if (input$FFTshifted) {
            handles$magnminimumsf <- (rv$f[xmins[, 2]] / pi)
          }
          else {
            if (input$twosidedFFT) {
              handles$magnminimumsf <- rv$f[xmins[, 2]] / (2 * pi)
            }
            else {
              handles$magnminimumsf <- rv$f[xmins[, 2]] / (pi)
            }
          }
        }
        if (is.vector(xmins)) {
          if (input$logarithmicMagPlotAmplitude) {
            if (input$FFTshifted) {
              handles$magnminimumsa <- 20 * log10(xmins[1])
            }
            else {
              handles$magnminimumsa <- 20 * log10(xmins[1])
            }
            if (input$normalizedMagPlotAmplitude) {
              handles$magnminimumsa <- isolate(handles$magnminimumsa) -
                20 * log10(max(Mod(rv$h), na.rm = TRUE))
            }
          }
          else {
            if (input$FFTshifted) {
              handles$magnminimumsa <- xmins[1]
            }
            else {
              handles$magnminimumsa <- xmins[1]
            }
            if (input$normalizedMagPlotAmplitude) {
              handles$magnminimumsa <-
                isolate(handles$magnminimumsa) / max(Mod(rv$h),
                                                     na.rm = TRUE)
            }
            else {
              handles$magnminimumsa <-
                isolate(handles$magnminimumsa) / input$edit_gain
            }
          }
        }
        else {
          if (input$logarithmicMagPlotAmplitude) {
            if (input$FFTshifted) {
              handles$magnminimumsa <- 20 * log10(xmins[, 1])
            }
            else {
              handles$magnminimumsa <- 20 * log10(xmins[, 1])
            }
            if (input$normalizedMagPlotAmplitude) {
              handles$magnminimumsa <- isolate(handles$magnminimumsa) -
                20 * log10(max(Mod(rv$h), na.rm = TRUE))
            }
          }
          else {
            if (input$FFTshifted) {
              handles$magnminimumsa <- xmins[, 1]
            }
            else {
              handles$magnminimumsa <- xmins[, 1]
            }
            if (input$normalizedMagPlotAmplitude) {
              handles$magnminimumsa <-
                isolate(handles$magnminimumsa) / max(Mod(rv$h),
                                                     na.rm = TRUE)
            }
            else {
              handles$magnminimumsa <-
                isolate(handles$magnminimumsa) / input$edit_gain
            }
          }
        }
        points(
          handles$magnminimumsf,
          handles$magnminimumsa,
          lty = "dotted",
          lwd = input$LineWidth,
          pch = 25L,
          bg = input$BackgroundColor,
          col = "maroon"
        )
      }
    }
  })
  
  # output$axes_magpassbandstopband ----
  output$axes_magpassbandstopband <-
    renderPlot(width = "auto", height = "auto",
               {
                 filtb <- handlesb()
                 filta <-
                   handlesa()
                 filtb[Re(filtb) > 1e+12] <-
                   1e+12
                 filtb[Re(filtb) < -1e+12] <-
                   -1e+12
                 filta[Re(filta) > 1e+12] <-
                   1e+12
                 filta[Re(filta) < -1e+12] <-
                   -1e+12
                 filtb[Im(filtb) > 1e+12] <-
                   Re(filtb) + 1i * 1e+12
                 filtb[Im(filtb) < -1e+12] <-
                   Re(filtb) - 1i * 1e+12
                 filta[Im(filta) > 1e+12] <-
                   Re(filtb) + 1i * 1e+12
                 filta[Im(filta) < -1e+12] <-
                   Re(filtb) - 1i * 1e+12
                 rv <-
                   signal::freqz(
                     filtb,
                     filta,
                     region = "whole",
                     n = 2L ^ 20L,
                     Fs = 2 * pi * input$samplingfreq /
                       2L
                   )
                 par(mgp = c(2.5, 1, 0))
                 par(mfrow = c(1, 2))
                 plot(
                   if (input$FFTshifted) {
                     (rv$f / pi - input$samplingfreq / 2)
                   }
                   else {
                     if (input$twosidedFFT) {
                       rv$f / (2 * pi)
                     }
                     else {
                       rv$f / (pi)
                     }
                   },
                   if (input$logarithmicMagPlotAmplitude) {
                     (if (input$FFTshifted) {
                       20 * log10(Mod(pracma::fftshift(rv$h)))
                     }
                     else {
                       20 * log10(Mod(rv$h))
                     }) - (if (input$normalizedMagPlotAmplitude) {
                       20 * log10(max(Mod(rv$h), na.rm = TRUE))
                     }
                     else {
                       20 * log10(1/ input$edit_gain)
                     })
                   }
                   else {
                     (if (input$FFTshifted) {
                       Mod(pracma::fftshift(rv$h))
                     }
                     else {
                       Mod(rv$h)
                     }) / (if (input$normalizedMagPlotAmplitude) {
                       max(Mod(rv$h), na.rm = TRUE)
                     }
                     else {
                       1/ input$edit_gain
                     })
                   },
                   xlim = input$zoomlimXpassband,
                   ylim = input$zoomlimYpassband,
                   type = "l",
                   col = input$ForegroundColor,
                   bg = input$BackgroundColor,
                   lwd = input$LineWidth,
                   xlab = if (input$freqaxisunits == "zero2one") {
                     expression("Normalized Frequency (" %*% pi ~ ~ "[rads/Sample])")
                   }
                   else if (input$freqaxisunits == "zero2pi") {
                     expression(omega ~ ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero22pi") {
                     expression(omega ~ ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero2half") {
                     expression(omega / {
                       2 * pi
                     } ~ ~  ~  ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero2piby2") {
                     expression(2L * omega ~ ~  ~  ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero2fs") {
                     if (input$twosidedFFT)
                       expression(-f[s] %->% ~
                                    ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                     else
                       expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                   }
                   else if (input$freqaxisunits == "zero2fsby2") {
                     if ((!input$twosidedFFT) &&
                         (!input$FFTshifted) && (!input$logarithmicFreqAxis)) {
                       expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                     }
                     else {
                       expression(0L %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
                     }
                   }
                   else if (input$freqaxisunits == "zero2fmax") {
                     expression(0L %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
                   }
                   else if (input$freqaxisunits == "zero2fmaxby2") {
                     if (input$twosidedFFT)
                       expression(-f[max] /
                                    2L %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
                     else
                       expression(0L %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
                   },
                   ylab = if ((input$freqaxisunits == "zero2one") ||
                              (input$freqaxisunits == "zero2pi") ||
                              (input$freqaxisunits == "zero22pi") ||
                              (input$freqaxisunits == "zero2half") ||
                              (input$freqaxisunits == "zero2piby2")) {
                     if (input$normalizedMagPlotAmplitude) {
                       if (input$logarithmicMagPlotAmplitude) {
                         expression(paste(group("|", bold(H(
                           e ^ {
                             1i * omega
                           }
                         )), "|"), " / max [dB]"))
                       }
                       else {
                         expression(paste(group("|", bold(H(
                           e ^ {
                             1i * omega
                           }
                         )), "|")), " / max")
                       }
                     }
                     else {
                       if (input$logarithmicMagPlotAmplitude) {
                         expression(paste(group("|", bold(H(
                           e ^ {
                             1i * omega
                           }
                         )), "|"), "  [dB]"))
                       }
                       else {
                         expression(group("|", bold(H(e ^ {
                           1i * omega
                         })), "|"))
                       }
                     }
                   }
                   else {
                     if (input$logarithmicMagPlotAmplitude) {
                       expression(paste(group("|", bold(H(
                         e ^ {
                           j ~ ~ 2 * pi * f
                         }
                       )), "|"), "[dB]"))
                     }
                     else {
                       expression(group("|", bold(H(e ^ {
                         j ~ ~ 2 * pi * f
                       })), "|"))
                     }
                   },
                   main = "Pass-Band"
                 )
                 if (input$checkboxRAY)
                   abline(
                     v = input$slider1,
                     lwd = input$LineWidth,
                     col = "magenta",
                     lty = "dashed"
                   )
                 grid(col = input$grcolor)
                 abline(h = 0L, col = "black")
                 abline(v = 0L, col = "black")
                 if (input$secondaryaxis) {
                   if ((input$freqaxisunits == "zero2pi") || 
                       (input$freqaxisunits == "zero22pi") ||
                       (input$freqaxisunits == "zero2piby2")) {
                     axis(
                       side = 3L,
                       at = pi * c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
                       labels = expression(-6*pi,-11*pi/2,                         -5 * pi,                         -9 * pi / 2,                         -4 * pi,                         -7 * pi / 2,-3 * pi,                         -5 * pi / 2,                         -2 * pi,                         -3 * pi / 2,                         -pi,                         -pi / 2,-pi / 3,                         -pi / 5,                         pi / 5,                         pi / 3,                         pi / 2,                         pi,                         3 * pi / 2,                         2 * pi,                         5 * pi /                           2L,                         3 * pi,                         7 * pi / 2,                         4 * pi,                         9 * pi / 2,                         5 * pi,                         11 * pi /                           2L,                         6 * pi                       ),
                       tick = TRUE,
                       pos = par("usr")[4],
                       outer = FALSE,
                       col = input$grcolor,
                       cex.axis = 0.8,
                       mgp = c(-2.5,-1L, 0L),
                       xlab = "pi-based"
                     )
                   }
                   if ((input$freqaxisunits == "zero2one") ||
                       (input$freqaxisunits == "zero2half")) {
                     axis(
                       side = 3L,
                       at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
                       labels = expression(-6*pi,-11*pi/2,                         -5 * pi,                         -9 * pi / 2,                         -4 * pi,                         -7 * pi / 2,-3 * pi,                         -5 * pi / 2,                         -2 * pi,                         -3 * pi / 2,                         -pi,                         -pi / 2,-pi / 3,                         -pi / 5,                         pi / 5,                         pi / 3,                         pi / 2,                         pi,                         3 * pi / 2,                         2 * pi,                         5 * pi /                           2L,                         3 * pi,                         7 * pi / 2,                         4 * pi,                         9 * pi / 2,                         5 * pi,                         11 * pi /                           2L,                         6 * pi                       ),
                       tick = TRUE,
                       pos = par("usr")[4],
                       outer = FALSE,
                       col = input$grcolor,
                       cex.axis = 0.8,
                       mgp = c(-2.5,-1L, 0L)
                     )
                   }
                   if ((input$freqaxisunits == "zero2fs") ||
                       (input$freqaxisunits == "zero2fmax") ||
                       (input$freqaxisunits == "zero2fmaxby2")) {
                     axis(
                       side = 3L,
                       at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L
                       ) * input$samplingfreq / 2,
                       labels = expression(-6*pi,-11*pi/2,                         -5 * pi,                         -9L *                           pi /                           2L,                         -4 * pi,                         -7 * pi / 2,                         -3 * pi,                         -5 * pi / 2,                         -2L *                           pi,                         -3 * pi / 2,                         -pi,                         -pi / 2,                         -pi / 3,                         -pi / 5,                         pi / 5,                         pi / 3,                         pi /                           2L,                         pi,                         3 * pi / 2,                         2 * pi,                         5 * pi / 2,                         3 * pi,                         7L *                           pi /                           2L,                         4 * pi,                         9 * pi / 2,                         5 * pi,                         11 * pi / 2,                         6L *                           pi                       ),
                       tick = TRUE,
                       line = -0.4,
                       pos = par("usr")[4],
                       outer = FALSE,
                       col = input$grcolor,
                       cex.axis = 0.8,
                       mgp = c(-2.5,-1L, 0L)
                     )
                   }
                 }
                 if (input$showPhaseOnMagPlot) {
                   lines(
                     if (input$FFTshifted) {
                       (rv$f / pi - input$samplingfreq / 2)
                     }
                     else {
                       if (input$twosidedFFT) {
                         rv$f / (2 * pi)
                       }
                       else {
                         rv$f / (pi)
                       }
                     },
                     if (input$FFTshifted) {
                       Arg(pracma::fftshift(rv$h))
                     }
                     else {
                       Arg(rv$h)
                     },
                     col = "grey",
                     lwd = input$LineWidth * 2/3,
                     lty = "dotted"
                   )
                   if (input$showLegend)
                     legend(
                       "topright",
                       inset = 0.03,
                       c("magn.", "phase"),
                       col = c(input$ForegroundColor, "grey"),
                       lty = c("solid",
                               "dotted"),
                       bty = "n",
                       cex = 0.8
                     )
                 }
                 plot(
                   if (input$FFTshifted) {
                     (rv$f / pi - input$samplingfreq / 2)
                   }
                   else {
                     if (input$twosidedFFT) {
                       rv$f / (2 * pi)
                     }
                     else {
                       rv$f / (pi)
                     }
                   },
                   if (input$logarithmicMagPlotAmplitude) {
                     (if (input$FFTshifted) {
                       20 * log10(Mod(pracma::fftshift(rv$h)))
                     }
                     else {
                       20 * log10(Mod(rv$h))
                     }) - (if (input$normalizedMagPlotAmplitude) {
                       20 * log10(max(Mod(rv$h), na.rm = TRUE))
                     }
                     else {
                       0L
                     })
                   }
                   else {
                     (if (input$FFTshifted) {
                       Mod(pracma::fftshift(rv$h))
                     }
                     else {
                       Mod(rv$h)
                     }) / (if (input$normalizedMagPlotAmplitude) {
                       max(Mod(rv$h), na.rm = TRUE)
                     }
                     else {
                       1/ input$edit_gain
                     })
                   },
                   xlim = input$zoomlimXstopband,
                   ylim = input$zoomlimYstopband,
                   type = "l",
                   col = input$ForegroundColor,
                   bg = input$BackgroundColor,
                   lwd = input$LineWidth,
                   xlab = if (input$freqaxisunits == "zero2one") {
                     expression("Normalized Frequency (" %*% pi ~ ~ "[rads/Sample])")
                   }
                   else if (input$freqaxisunits == "zero2pi") {
                     expression(omega ~ ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero22pi") {
                     expression(omega ~ ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero2half") {
                     expression(omega / {
                       2 * pi
                     } ~ ~  ~  ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero2piby2") {
                     expression(2L * omega ~ ~  ~  ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero2fs") {
                     if (input$twosidedFFT)
                       expression(-f[s] %->% ~
                                    ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                     else
                       expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                   }
                   else if (input$freqaxisunits == "zero2fsby2") {
                     if ((!input$twosidedFFT) &&
                         (!input$FFTshifted) && (!input$logarithmicFreqAxis)) {
                       expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                     }
                     else {
                       expression(0L %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
                     }
                   }
                   else if (input$freqaxisunits == "zero2fmax") {
                     expression(0L %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
                   }
                   else if (input$freqaxisunits == "zero2fmaxby2") {
                     if (input$twosidedFFT)
                       expression(-f[max] /
                                    2L %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
                     else
                       expression(0L %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
                   },
                   ylab = if ((input$freqaxisunits == "zero2one") ||
                              (input$freqaxisunits == "zero2pi") ||
                              (input$freqaxisunits == "zero22pi") ||
                              (input$freqaxisunits == "zero2half") ||
                              (input$freqaxisunits == "zero2piby2")) {
                     if (input$normalizedMagPlotAmplitude) {
                       if (input$logarithmicMagPlotAmplitude) {
                         expression(paste(group("|", bold(H(
                           e ^ {
                             1i * omega
                           }
                         )), "|"), " / max [dB]"))
                       }
                       else {
                         expression(paste(group("|", bold(H(
                           e ^ {
                             1i * omega
                           }
                         )), "|")), " / max")
                       }
                     }
                     else {
                       if (input$logarithmicMagPlotAmplitude) {
                         expression(paste(group("|", bold(H(
                           e ^ {
                             1i * omega
                           }
                         )), "|"), "  [dB]"))
                       }
                       else {
                         expression(group("|", bold(H(e ^ {
                           1i * omega
                         })), "|"))
                       }
                     }
                   }
                   else {
                     if (input$logarithmicMagPlotAmplitude) {
                       expression(paste(group("|", bold(H(
                         e ^ {
                           j ~ ~ 2 * pi * f
                         }
                       )), "|"), "[dB]"))
                     }
                     else {
                       expression(group("|", bold(H(e ^ {
                         j ~ ~ 2 * pi * f
                       })), "|"))
                     }
                   },
                   main = "Stop-Band"
                 )
                 if (input$checkboxRAY)
                   abline(
                     v = input$slider1,
                     lwd = input$LineWidth,
                     col = "magenta",
                     lty = "dashed"
                   )
                 grid(col = input$grcolor)
                 abline(h = 0L, col = "black")
                 abline(v = 0L, col = "black")
                 if (input$secondaryaxis) {
                   if ((input$freqaxisunits == "zero2pi") || 
                       (input$freqaxisunits == "zero22pi") ||
                       (input$freqaxisunits == "zero2piby2")) {
                     axis(
                       side = 3L,
                       at = pi * c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
                       labels = expression(-6*pi,-11*pi/2,                         -5 * pi,                         -9 * pi / 2,                         -4 * pi,                         -7 * pi / 2,-3 * pi,                         -5 * pi / 2,                         -2 * pi,                         -3 * pi / 2,                         -pi,                         -pi / 2,-pi / 3,                         -pi / 5,                         pi / 5,                         pi / 3,                         pi / 2,                         pi,                         3 * pi / 2,                         2 * pi,                         5 * pi /                           2L,                         3 * pi,                         7 * pi / 2,                         4 * pi,                         9 * pi / 2,                         5 * pi,                         11 * pi /                           2L,                         6 * pi                       ),
                       tick = TRUE,
                       pos = par("usr")[4],
                       outer = FALSE,
                       col = input$grcolor,
                       cex.axis = 0.8,
                       mgp = c(-2.5,-1L, 0L),
                       xlab = "pi-based"
                     )
                   }
                   if ((input$freqaxisunits == "zero2one") ||
                       (input$freqaxisunits == "zero2half")) {
                     axis(
                       side = 3L,
                       at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
                       labels = expression(-6*pi,-11*pi/2,                         -5 * pi,                         -9 * pi / 2,                         -4 * pi,                         -7 * pi / 2,-3 * pi,                         -5 * pi / 2,                         -2 * pi,                         -3 * pi / 2,                         -pi,                         -pi / 2,-pi / 3,                         -pi / 5,                         pi / 5,                         pi / 3,                         pi / 2,                         pi,                         3 * pi / 2,                         2 * pi,                         5 * pi /                           2L,                         3 * pi,                         7 * pi / 2,                         4 * pi,                         9 * pi / 2,                         5 * pi,                         11 * pi /                           2L,                         6 * pi                       ),
                       tick = TRUE,
                       pos = par("usr")[4],
                       outer = FALSE,
                       col = input$grcolor,
                       cex.axis = 0.8,
                       mgp = c(-2.5,-1L, 0L)
                     )
                   }
                   if ((input$freqaxisunits == "zero2fs") ||
                       (input$freqaxisunits == "zero2fmax") ||
                       (input$freqaxisunits == "zero2fmaxby2")) {
                     axis(
                       side = 3L,
                       at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L
                       ) * input$samplingfreq / 2,
                       labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
                       tick = TRUE,
                       line = -0.4,
                       pos = par("usr")[4],
                       outer = FALSE,
                       col = input$grcolor,
                       cex.axis = 0.8,
                       mgp = c(-2.5,-1L, 0L)
                     )
                   }
                 }
                 if (input$showPhaseOnMagPlot) {
                   lines(
                     if (input$FFTshifted) {
                       (rv$f / pi - input$samplingfreq / 2)
                     }
                     else {
                       if (input$twosidedFFT) {
                         rv$f / (2 * pi)
                       }
                       else {
                         rv$f / (pi)
                       }
                     },
                     if (input$FFTshifted) {
                       Arg(pracma::fftshift(rv$h))
                     }
                     else {
                       Arg(rv$h)
                     },
                     col = "grey",
                     lwd = input$LineWidth * 2/3,
                     lty = "dotted"
                   )
                   if (input$showLegend)
                     legend(
                       "topright",
                       inset = 0.03,
                       c("magn.", "phase"),
                       col = c(input$ForegroundColor, "grey"),
                       lty = c("solid",
                               "dotted"),
                       bty = "n",
                       cex = 0.8
                     )
                 }
                 par(mfrow = c(1, 1))
               })
  
  # output$axes_phase ----
  output$axes_phase <- renderPlot(width = "auto", height = "auto", {
    filtb <- handlesb()
    filta <- handlesa()
    filtb[Re(filtb) > 1e+12] <- 1e+12
    filtb[Re(filtb) < -1e+12] <- -1e+12
    filta[Re(filta) > 1e+12] <- 1e+12
    filta[Re(filta) < -1e+12] <- -1e+12
    filtb[Im(filtb) > 1e+12] <- Re(filtb) + 1i * 1e+12
    filtb[Im(filtb) < -1e+12] <- Re(filtb) - 1i * 1e+12
    filta[Im(filta) > 1e+12] <- Re(filtb) + 1i * 1e+12
    filta[Im(filta) < -1e+12] <- Re(filtb) - 1i * 1e+12
    rv <-
      signal::freqz(
        filtb,
        filta,
        region = "whole",
        n = 2L ^ 20L,
        Fs = 2L *
          pi * input$samplingfreq / 2
      )
    plot(
      if (input$FFTshifted) {
        (rv$f / pi - input$samplingfreq / 2)
      }
      else {
        if (input$twosidedFFT) {
          rv$f / (2 * pi)
        }
        else {
          rv$f / (pi)
        }
      },
      if (input$unwrapPhase) {
        if (input$FFTshifted) {
          unWrap(Arg(pracma::fftshift(rv$h)), tol = (pi - eps)) / (if (input$degreesgrid) {
            pi / 180
          }
          else {
            pi
          })
        }
        else {
          unWrap(Arg(rv$h), tol = (pi - eps)) / (if (input$degreesgrid) {
            pi / 180
          }
          else {
            pi
          })
        }
      }
      else {
        if (input$FFTshifted) {
          Arg(pracma::fftshift(rv$h)) / (if (input$degreesgrid) {
            pi / 180
          }
          else {
            pi
          })
        }
        else {
          Arg(rv$h) / (if (input$degreesgrid) {
            pi / 180
          }
          else {
            pi
          })
        }
      },
      xlim = c(if (input$twosidedFFT) {
        -input$samplingfreq / 2
      } else {
        0L
      }, if (input$FFTshifted) {
        input$samplingfreq / 2
      } else {
        if (input$twosidedFFT) {
          input$samplingfreq / 2
        } else {
          2L * input$samplingfreq / 2
        }
      }),
      ylim = if (input$unwrapPhase) {
        NULL
      }
      else if (input$degreesgrid) {
        NULL
      }
      else {
        c(-1L, 1L)
      },
      type = "l",
      col = input$ForegroundColor,
      bg = input$BackgroundColor,
      lwd = input$LineWidth,
      xlab = if (input$freqaxisunits == "zero2one") {
        expression("Normalized Frequency (" %*% pi ~ ~ "[rads/Sample])")
      }
      else if (input$freqaxisunits == "zero2pi") {
        expression(omega ~ ~ "[rads]")
      }
      else if (input$freqaxisunits == "zero22pi") {
        expression(omega ~ ~ "[rads]")
      }
      else if (input$freqaxisunits == "zero2half") {
        expression(omega / {
          2 * pi
        } ~ ~  ~  ~ "[rads]")
      }
      else if (input$freqaxisunits == "zero2piby2") {
        expression(2L * omega ~ ~  ~  ~ "[rads]")
      }
      else if (input$freqaxisunits == "zero2fs") {
        if (input$twosidedFFT)
          expression(-f[s] %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
        else
          expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
      }
      else if (input$freqaxisunits == "zero2fsby2") {
        if ((!input$twosidedFFT) &&
            (!input$FFTshifted) && (!input$logarithmicFreqAxis)) {
          expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
        }
        else {
          expression(0L %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
        }
      }
      else if (input$freqaxisunits == "zero2fmax") {
        expression(0L %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
      }
      else if (input$freqaxisunits == "zero2fmaxby2") {
        if (input$twosidedFFT)
          expression(-f[max] / 2 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
        else
          expression(0L %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
      },
      ylab = if ((input$freqaxisunits == "zero2one") ||
                 (input$freqaxisunits == "zero2pi") ||
                 (input$freqaxisunits == "zero22pi") || 
                 (input$freqaxisunits == "zero2half") ||
                 (input$freqaxisunits == "zero2piby2")) {
        if (input$degreesgrid) {
          expression(paste("Angle ", bold(H(e ^ {
            1i * omega
          }))))
        }
        else {
          expression(paste("Angle ", bold(H(e ^ {
            1i * omega
          }))) %*% pi)
        }
      }
      else {
        expression(paste("Angle ", bold(H(e ^ {
          j ~ ~ 2 * pi * f
        }))))
      },
      main = paste0("Phase-Response", if (!input$unwrapPhase) {
        " (principal-value)"
      })
    )
    if (input$checkboxRAY)
      abline(
        v = input$slider1,
        lwd = input$LineWidth,
        col = "magenta",
        lty = "dashed"
      )
    grid(col = input$grcolor)
    abline(h = 0L, col = "black")
    abline(v = 0L, col = "black")
    if (input$secondaryaxis) {
      axis(
        side = 4L,
        at = if (input$degreesgrid) {
          180 * c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L)
        }
        else {
          c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L)
        },
        labels = expression(
          -6 * pi,
          -frac(11 * pi, 2L),
          -5 * pi,
          -frac(9 * pi, 2L),
          -4 * pi,
          -frac(7 * pi, 2L),
          -3 * pi,
          -frac(5 * pi, 2L),
          -2 * pi,
          -frac(3 * pi, 2L),
          -pi,
          frac(-pi, 2L),
          frac(-pi, 3L),
          frac(-pi, 5L),
          frac(pi, 5L),
          frac(pi, 3L),
          frac(pi, 2L),
          pi,
          frac(3 * pi, 2L),
          2 * pi,
          frac(5 * pi, 2L),
          3 * pi,
          frac(7 * pi, 2L),
          4 * pi,
          frac(9 * pi, 2L),
          5 * pi,
          frac(11 * pi, 2L),
          6 * pi
        ),
        tick = TRUE,
        pos = par("usr")[2],
        outer = FALSE,
        col = input$grcolor,
        cex.axis = 0.8,
        las = 1L
      )
      if ((input$freqaxisunits == "zero2pi") ||
          (input$freqaxisunits == "zero22pi") ||
          (input$freqaxisunits == "zero2piby2")) {
        axis(
          side = 3L,
          at = pi * c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
          labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
          tick = TRUE,
          pos = par("usr")[4],
          outer = FALSE,
          col = input$grcolor,
          cex.axis = 0.8,
          mgp = c(-2.5,-1L,0L)
        )
      }
      if ((input$freqaxisunits == "zero2one") ||
          (input$freqaxisunits == "zero2half")) {
        axis(
          side = 3L,
          at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L),
          labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
          tick = TRUE,
          pos = par("usr")[4],
          outer = FALSE,
          col = input$grcolor,
          cex.axis = 0.8,
          mgp = c(-2.5,-1L,0L)
        )
      }
      if ((input$freqaxisunits == "zero2fs") ||
          (input$freqaxisunits == "zero2fmax") ||
          (input$freqaxisunits == "zero2fmaxby2")) {
        axis(
          side = 3L,
          at = c(-6L,-5.5,-5L,-4.5,-4L,-3.5,-3L,-2.5,-2L,-1.5,-1L,-1/2,-1/3,-1/5,1/5,1/3,1/2,1L,1.5,2L,2.5,3L,3.5,4L,4.5,5L,5.5,6L
          ) * input$samplingfreq / 2,
          labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
          tick = TRUE,
          line = -0.4,
          pos = par("usr")[4],
          outer = FALSE,
          col = input$grcolor,
          cex.axis = 0.8,
          mgp = c(-2.5,-1L, 0L)
        )
      }
    }
  })
  
  # output$axes_grpdelay ----
  output$axes_grpdelay <-
    renderPlot(width = "auto", height = "auto",
               {
                 rv <- signal::grpdelay(
                   handlesb(),
                   handlesa(),
                   whole = TRUE,
                   n = 512,
                   Fs = 2 * pi * input$samplingfreq / 2
                 )
                 plot(
                   if (input$FFTshifted) {
                     (rv$w / pi - input$samplingfreq / 2)
                   }
                   else {
                     if (input$twosidedFFT) {
                       rv$w / (2 * pi)
                     }
                     else {
                       rv$w / (pi)
                     }
                   },
                   if (input$unwrapPhase) {
                     if (input$FFTshifted) {
                       pracma::fftshift(rv$gd)
                     }
                     else {
                       rv$gd
                     }
                   }
                   else {
                     if (input$FFTshifted) {
                       pracma::fftshift(rv$gd)
                     }
                     else {
                       rv$gd
                     }
                   },
                   xlim = c(if (input$twosidedFFT) {
                     -input$samplingfreq / 2
                   } else {
                     0L
                   }, if (input$FFTshifted) {
                     input$samplingfreq / 2
                   } else {
                     if (input$twosidedFFT) {
                       input$samplingfreq / 2
                     } else {
                       2L * input$samplingfreq / 2
                     }
                   }),
                   ylim = c(max(c(
                     -10L, min(rv$gd, max(rv$gd, na.rm = TRUE) -
                                1L, na.rm = TRUE)
                   ), na.rm = TRUE), min(c(
                     100L, max(rv$gd,
                              min(rv$gd, na.rm = TRUE) + 1L, na.rm = TRUE)
                   ), na.rm = TRUE)),
                   type = "l",
                   col = input$ForegroundColor,
                   bg = input$BackgroundColor,
                   lwd = input$LineWidth,
                   xlab = if (input$freqaxisunits == "zero2one") {
                     expression("Normalized Frequency (" %*% pi ~ ~ "[rads/Sample])")
                   }
                   else if (input$freqaxisunits == "zero2pi") {
                     expression(omega ~ ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero22pi") {
                     expression(omega ~ ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero2half") {
                     expression(omega / {
                       2 * pi
                     } ~ ~  ~  ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero2piby2") {
                     expression(2L * omega ~ ~  ~  ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero2fs") {
                     if (input$twosidedFFT)
                       expression(-f[s] %->% ~  ~  ~
                                    ~ f[s] ~ ~  ~  ~ "[Hz]")
                     else
                       expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                   }
                   else if (input$freqaxisunits == "zero2fsby2") {
                     if ((!input$twosidedFFT) &&
                         (!input$FFTshifted) && 
                         (!input$logarithmicFreqAxis)
                         ) {
                       expression(0L %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                     }
                     else {
                       expression(0L %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
                     }
                   }
                   else if (input$freqaxisunits == "zero2fmax") {
                     expression(0L %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
                   }
                   else if (input$freqaxisunits == "zero2fmaxby2") {
                     if (input$twosidedFFT)
                       expression(-f[max] / 2 %->% ~
                                    ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
                     else
                       expression(0L %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
                   },
                   ylab = if ((input$freqaxisunits == "zero2one") ||
                              (input$freqaxisunits == "zero2pi") ||
                              (input$freqaxisunits == "zero22pi") ||
                              (input$freqaxisunits == "zero2half") ||
                              (input$freqaxisunits == "zero2piby2")
                              ) {
                     if (input$degreesgrid) {
                       expression(paste({
                         d * varphi
                       } / {
                         d * omega
                       } ~ ~  ~  ~ "[Samples]"))
                     }
                     else {
                       expression(paste({
                         d * varphi
                       } / {
                         d * omega
                       } ~ ~  ~  ~ "[Samples]"))
                     }
                   }
                   else {
                     expression(paste({
                       d * varphi
                     } / {
                       d * omega
                     } ~ ~  ~  ~ "[Samples]"))
                   },
                   main = "Group-Delay"
                 )
                 if (input$checkboxRAY)
                   abline(
                     v = input$slider1,
                     lwd = input$LineWidth,
                     col = "magenta",
                     lty = "dashed"
                   )
                 grid(col = input$grcolor)
                 abline(h = 0L, col = "black")
                 abline(v = 0L, col = "black")
               })
  
  # output$axes_imp ----
  output$axes_imp <-
    renderPlot(width = "auto",
               height = "auto",
               expr = {
                 plot_imp(handleshn(), hnimag(), handles$poleloc, input, output)
                 if (stabilityCheck(handlesa())) {
                   output$system_stable <- renderUI({
                     tags$span(style = paste0("color:", input$ForegroundColor),
                               "stable-system")
                   })
                 }
                 else {
                   output$system_stable <- renderUI({
                     tags$span(style = "color:red", "unstable-system")
                   })
                   box(
                     which = "plot",
                     col = MyColourForUnstableSystem,
                     lwd = 3L * input$LineWidth,
                     lty = "dashed"
                   )
                   box(
                     which = "inner",
                     col = MyColourForUnstableSystem,
                     lwd = 3L * input$LineWidth,
                     lty = "dotted"
                   )
                   grid(col = MyColourForUnstableSystem)
                   abline(h = 0L, col = MyColourForUnstableSystem)
                   abline(v = 0L, col = MyColourForUnstableSystem)
                 }
               })

    # output$axes_step ----
  output$axes_step <-
    renderPlot(width = "auto",
               height = "auto",
               expr = {
                 plot_step(handleshnu(), hnimagu(), handles$poleloc, input, output)
                 if (stabilityCheck(handlesa())) {
                   output$system_stable <- renderUI({
                     tags$span(style = paste0("color:", input$ForegroundColor),
                               "stable-system")
                   })
                 }
                 else {
                   output$system_stable <- renderUI({
                     tags$span(style = "color:red", "unstable-system")
                   })
                   box(
                     which = "plot",
                     col = MyColourForUnstableSystem,
                     lwd = 3L * input$LineWidth,
                     lty = "dashed"
                   )
                   box(
                     which = "inner",
                     col = MyColourForUnstableSystem,
                     lwd = 3L *
                       input$LineWidth,
                     lty = "dotted"
                   )
                   grid(col = MyColourForUnstableSystem)
                   abline(h = 0L, col = MyColourForUnstableSystem)
                   abline(v = 0L, col = MyColourForUnstableSystem)
                 }
               })
  
  # output$axes_acf ----
  output$axes_acf <- renderPlot(width = "auto", height = "auto", {
    par(mfrow = c(2, 1))
    acf(Re(handleshn()),
        main = "Auto-Correlation Function of Re(h[n])",
        xlim = c(0L, if (!is.null(handleshn())) {
          min(length(handleshn()) - 1L, 10L * log10(length(handleshn())))
        } else {
          1L
        }))
    pacf(Re(handleshn()),
         main = "Partial Auto-Correlation Function of Re(h[n])",
         xlim = c(0L, if (!is.null(handleshn())) {
           min(length(handleshn()) - 1L, 10L * log10(length(handleshn())))
         } else {
           1L
         }))
    par(mfrow = c(1, 1))
  })
  
  # Render Transfer-Function ----
  output$transferfn <- renderUI({
    withMathJax(
      paste0(
        "$$\\begin{aligned}", # \\label{eq:txfrfn}",
        "H(z) =",
        "\\frac{Y(z)}{X(z)} &= b_0\\cdot\\frac{",
        {
          accumulatorstring <- NULL
          for (i in 1L:length(handles$zeroloc)) {
            switch(
              abs(sign(Re(
                handles$zeroloc[i]
              ))) + 1L,
              accumulatorstring <- paste0(accumulatorstring,
                                          paste0(if (i > 1L) {
                                            "\\cdot "
                                          }, if (abs(Im(handles$zeroloc[i])) <= 1e-06) {
                                            # "z"
                                            paste0("z^{",max(c(length(handles$zeroloc),length(handles$poleloc))),"}")
                                          } else {
                                            paste0(
                                              "(z-\\overbrace{z_{[",
                                              i,
                                              "]}}^{",
                                              gsub("i",
                                                   "\\\\jmath", stripImagZero(round(
                                                     handles$zeroloc[i],
                                                     max(c(
                                                       2L,
                                                       countZeroDigitsRightOfDecimalPoint(handles$zeroloc[i])
                                                     ),na.rm=TRUE)
                                                   ))),
                                              "})"
                                            )
                                          })),
              accumulatorstring <- paste0(
                accumulatorstring,
                if (i > 1L) {
                  "\\cdot "
                },
                "(z-\\overbrace{z_{[",
                i,
                "]}}^{",
                gsub("i", "\\\\jmath",
                     stripImagZero(round(
                       handles$zeroloc[i], max(c(
                         2L,
                         countZeroDigitsRightOfDecimalPoint(handles$zeroloc[i])
                       ),na.rm=TRUE)
                     ))),
                "})"
              )
            )
          }
          accumulatorstring
        },
        "}{",
        {
          accumulatorstring <- NULL
          for (i in 1L:length(handles$poleloc)) {
            switch(
              abs(sign(Re(
                handles$poleloc[i]
              ))) + 1L,
              accumulatorstring <- paste0(accumulatorstring,
                                          if (i > 1L) {
                                            "\\cdot "
                                          }, if (abs(Im( handles$poleloc[i] )) <= 1e-06) {
                                            # "z"
                                            paste0("z^{",max(c(length(handles$zeroloc),length(handles$poleloc))),"}")
                                          } else {
                                            paste0(
                                              "(z-\\underbrace{p_{[",
                                              i,
                                              "]}}_{",
                                              gsub("i",
                                                   "\\\\jmath", stripImagZero(round(
                                                     handles$poleloc[i],
                                                     max(c(
                                                       2L,
                                                       countZeroDigitsRightOfDecimalPoint(handles$poleloc[i])
                                                     ),na.rm=TRUE)
                                                   ))),
                                              "})"
                                            )
                                          }),
              accumulatorstring <- paste0(
                accumulatorstring,
                if (i > 1L) {
                  "\\cdot "
                },
                "(z-\\underbrace{p_{[",
                i,
                "]}}_{",
                gsub("i", "\\\\jmath",
                     stripImagZero(round(
                       handles$poleloc[i], max(c(
                         2L,
                         countZeroDigitsRightOfDecimalPoint(handles$poleloc[i])
                       ),na.rm=TRUE)
                     ))),
                "})"
              )
            )
          }
          accumulatorstring
        },
        if ((length(handles$zeroloc) + length(handles$poleloc) >
             8L) &&
            (length(handles$poleloc) > 2L)) {
          "}\\\\&=" # new-line
        }
        else {
          "}="
        },
        "\\overbrace{",
        gsub("i", "\\\\jmath", round(input$edit_gain,
                                     max(
                                       c(5, countZeroDigitsRightOfDecimalPoint(input$edit_gain))
                                     ,na.rm=TRUE))),
        "}^{G\\equiv b_{[0]}}\\cdot\\frac{",
        {
          accumulatorstring <- NULL
          for (i in 1L:length(handles$zeroloc)) {
            switch(
              abs(sign(Re(
                handles$zeroloc[i]
              ))) + 1L,
              accumulatorstring <- paste0(accumulatorstring,
                                          if (i > 1L) {
                                            "\\cdot "
                                          }, if (abs(Im( handles$zeroloc[i] )) <= 1e-06) {
                                            # "z"
                                            paste0("z^{",max(c(length(handles$zeroloc),length(handles$poleloc))),"}")
                                          } else {
                                            paste0(
                                              "(z-\\overbrace{(",
                                              gsub("i", "\\\\jmath",
                                                   stripImagZero(round(
                                                     handles$zeroloc[i], max(c(
                                                       2,
                                                       countZeroDigitsRightOfDecimalPoint(handles$zeroloc[i])
                                                     ),na.rm=TRUE)
                                                   ))),
                                              ")}^{z_{[",
                                              i,
                                              "]}",
                                              if (i > 1L) {
                                                {
                                                  subaccumulatorstring <- NULL
                                                  for (k in seq(1L, (i - 1L), by = 1L)) {
                                                    if (((is.infinite(Re(
                                                      handles$zeroloc[i]
                                                    ))) &&
                                                    (is.infinite(Re(
                                                      handles$zeroloc[k]
                                                    )))) ||
                                                    ((is.infinite(Im(
                                                      handles$zeroloc[i]
                                                    ))) &&
                                                    (is.infinite(Im(
                                                      handles$zeroloc[k]
                                                    ))))) {
                                                      break
                                                    } else {
                                                      if ((abs(
                                                        Re(handles$zeroloc[i]) - Re(handles$zeroloc[k])
                                                      ) <
                                                      1e-06) &&
                                                      (abs(
                                                        Im(handles$zeroloc[i]) -
                                                        Im(handles$zeroloc[k])
                                                      ) < 1e-06)) {
                                                        subaccumulatorstring <- paste0(subaccumulatorstring,
                                                                                       "=z_{[", k, "]}")
                                                        break
                                                      } else if ((abs(
                                                        Re(handles$zeroloc[i]) -
                                                        Re(handles$zeroloc[k])
                                                      ) < 1e-06) && (abs(
                                                        Im(handles$zeroloc[i]) +
                                                        Im(handles$zeroloc[k])
                                                      ) < 1e-06)) {
                                                        subaccumulatorstring <- paste0(subaccumulatorstring,
                                                                                       "=z^{*}_{[",
                                                                                       k,
                                                                                       "]}")
                                                        break
                                                      }
                                                    }
                                                  }
                                                  subaccumulatorstring
                                                }
                                              },
                                              "})"
                                            )
                                          }),
              accumulatorstring <- paste0(
                accumulatorstring,
                if (i > 1L) {
                  "\\cdot "
                },
                "(z-\\overbrace{(",
                gsub("i", "\\\\jmath", stripImagZero(round(
                  handles$zeroloc[i],
                  max(c(
                    2L,
                    countZeroDigitsRightOfDecimalPoint(handles$zeroloc[i])
                  ),na.rm=TRUE)
                ))),
                ")}^{z_{[",
                i,
                "]}",
                if (i > 1L) {
                  {
                    subaccumulatorstring <- NULL
                    for (k in seq(1L, (i - 1L), by = 1L)) {
                      if (((is.infinite(Re(
                        handles$zeroloc[i]
                      ))) &&
                      (is.infinite(Re(
                        handles$zeroloc[k]
                      )))) || ((is.infinite(Im(
                        handles$zeroloc[i]
                      ))) &&
                      (is.infinite(Im(
                        handles$zeroloc[k]
                      ))))) {
                        break
                      } else {
                        if ((abs(Re(handles$zeroloc[i]) - Re(handles$zeroloc[k])) <
                             1e-06) &&
                            (abs(Im(handles$zeroloc[i]) - Im(handles$zeroloc[k])) <
                             1e-06)) {
                          subaccumulatorstring <- paste0(subaccumulatorstring,
                                                         "=z_{[", k, "]}")
                          break
                        } else if ((abs(Re(handles$zeroloc[i]) - Re(handles$zeroloc[k])) <
                                    1e-06) &&
                                   (abs(Im(handles$zeroloc[i]) + Im(handles$zeroloc[k])) <
                                    1e-06)) {
                          subaccumulatorstring <- paste0(subaccumulatorstring,
                                                         "=z^{*}_{[", k, "]}")
                          break
                        }
                      }
                    }
                    subaccumulatorstring
                  }
                },
                "})"
              )
            )
          }
          accumulatorstring
        },
        "}{",
        {
          accumulatorstring <- NULL
          for (i in 1L:length(handles$poleloc)) {
            switch(
              abs(sign(Re(
                handles$poleloc[i]
              ))) + 1L,
              accumulatorstring <- paste0(accumulatorstring,
                                          if (i > 1L) {
                                            "\\cdot "
                                          }, if (abs(Im( handles$poleloc[i] )) <= 1e-06) {
                                            # "z"
                                            paste0("z^{",max(c(length(handles$zeroloc),length(handles$poleloc))),"}")
                                          } else {
                                            paste0(
                                              "(z-\\underbrace{(",
                                              gsub("i", "\\\\jmath",
                                                   stripImagZero(round(
                                                     handles$poleloc[i], max(c(
                                                       2L,
                                                       countZeroDigitsRightOfDecimalPoint(handles$poleloc[i])
                                                     ),na.rm=TRUE)
                                                   ))),
                                              ")}_{p_{[",
                                              i,
                                              "]}",
                                              if (i > 1L) {
                                                subaccumulatorstring <- NULL
                                                for (k in seq(1L, (i - 1L), by = 1L)) {
                                                  if (((is.infinite(Re(
                                                    handles$poleloc[i]
                                                  ))) &&
                                                  (is.infinite(Re(
                                                    handles$poleloc[k]
                                                  )))) ||
                                                  ((is.infinite(Im(
                                                    handles$poleloc[i]
                                                  ))) &&
                                                  (is.infinite(Im(
                                                    handles$poleloc[k]
                                                  ))))) {
                                                    break
                                                  } else {
                                                    if ((abs(Re(handles$poleloc[i]) - Re(handles$poleloc[k])) <
                                                         1e-06) &&
                                                        (abs(Im(handles$poleloc[i]) -
                                                             Im(handles$poleloc[k])) < 1e-06)) {
                                                      subaccumulatorstring <- paste0(subaccumulatorstring,
                                                                                     "=p_{[", k, "]}")
                                                      break
                                                    } else if ((abs(Re(handles$poleloc[i]) -
                                                                    Re(handles$poleloc[k])) < 1e-06) &&
                                                               (abs(Im(handles$poleloc[i]) +
                                                                    Im(handles$poleloc[k])) < 1e-06)) {
                                                      subaccumulatorstring <- paste0(subaccumulatorstring,
                                                                                     "=p^{*}_{[", k, "]}")
                                                      break
                                                    }
                                                  }
                                                }
                                                subaccumulatorstring
                                              },
                                              "})"
                                            )
                                          }),
              accumulatorstring <- paste0(
                accumulatorstring,
                if (i > 1L) {
                  "\\cdot "
                },
                "(z-\\underbrace{(",
                gsub("i", "\\\\jmath", stripImagZero(round(
                  handles$poleloc[i],
                  max(c(
                    2L,
                    countZeroDigitsRightOfDecimalPoint(handles$poleloc[i])
                  ),na.rm=TRUE)
                ))),
                ")}_{p_{[",
                i,
                "]}",
                if (i > 1L) {
                  subaccumulatorstring <- NULL
                  for (k in seq(1L, (i - 1L), by = 1L)) {
                    if (((is.infinite(Re(
                      handles$poleloc[i]
                    ))) && (is.infinite(Re(
                      handles$poleloc[k]
                    )))) ||
                    ((is.infinite(Im(
                      handles$poleloc[i]
                    ))) && (is.infinite(Im(
                      handles$poleloc[k]
                    ))))) {
                      break
                    } else {
                      if ((abs(Re(handles$poleloc[i]) - Re(handles$poleloc[k])) <
                           1e-06) &&
                          (abs(Im(handles$poleloc[i]) - Im(handles$poleloc[k])) <
                           1e-06)) {
                        subaccumulatorstring <- paste0(subaccumulatorstring,
                                                       "=p_{[", k, "]}")
                        break
                      } else if ((abs(Re(handles$poleloc[i]) - Re(handles$poleloc[k])) <
                                  1e-06) &&
                                 (abs(Im(handles$poleloc[i]) + Im(handles$poleloc[k])) <
                                  1e-06)) {
                        subaccumulatorstring <- paste0(subaccumulatorstring,
                                                       "=p^{*}_{[", k, "]}")
                        break
                      }
                    }
                  }
                  subaccumulatorstring
                },
                "})"
              )
            )
          }
          accumulatorstring
        },
        "}\\\\", # new-line
        "H(z) &=",
        if ((length(handles$zeroloc) <= 2L) &&
            (length(handles$poleloc) <= 2L)) {
          paste0(
            "\\overbrace{",
            gsub("i", "\\\\jmath", round(input$edit_gain,
                                         max(
                                           c(
                                             5L,
                                             countZeroDigitsRightOfDecimalPoint(input$edit_gain)
                                           )
                                         ,na.rm=TRUE))),
            "}^{G\\equiv b_{[0]}}\\cdot\\frac{",
            "1",
            if (length(handlesb()) >
                1L) {
              paste0(switch(
                abs(sign(Re(
                  handles$zeroloc[1]
                ))) + 1L,
                if (abs(Im(handles$zeroloc[1])) <= 1e-06) {
                  "+(0\\cdot z^{-1})"
                } else {
                  paste0(
                    "+ \\overbrace{\\left[-(",
                    gsub("i", "\\\\jmath",
                         stripImagZero(round(
                           handles$zeroloc[1], max(c(
                             2L,
                             countZeroDigitsRightOfDecimalPoint(handles$zeroloc[1])
                           ),na.rm=TRUE)
                         ))),
                    ")",
                    if (length(handles$zeroloc) > 1L) {
                      paste0("-(", gsub("i", "\\\\jmath", stripImagZero(
                        round(handles$zeroloc[2],
                              max(
                                c(
                                  2L,
                                  countZeroDigitsRightOfDecimalPoint(handles$zeroloc[2])
                                )
                              ,na.rm=TRUE))
                      )),
                      ")")
                    },
                    "\\right]}^{b_{[1]}/b_{[0]}=",
                    gsub("i", "\\\\jmath",
                         stripImagZero(
                           round(handlesb()[2] / handlesb()[1],
                                 max(
                                   c(
                                     2L,
                                     countZeroDigitsRightOfDecimalPoint(handlesb()[2] / handlesb()[1])
                                   )
                                 ,na.rm=TRUE))
                         )),
                    "}\\cdot z^{-1}"
                  )
                },
                paste0(
                  "+ \\overbrace{\\left[-(",
                  gsub("i", "\\\\jmath",
                       stripImagZero(round(
                         handles$zeroloc[1], max(c(
                           2L,
                           countZeroDigitsRightOfDecimalPoint(handles$zeroloc[1])
                         ),na.rm=TRUE)
                       ))),
                  ")",
                  if (length(handles$zeroloc) > 1L) {
                    paste0("-(", gsub("i", "\\\\jmath", stripImagZero(round(
                      handles$zeroloc[2],
                      max(c(
                        2L,
                        countZeroDigitsRightOfDecimalPoint(handles$zeroloc[2])
                      ),na.rm=TRUE)
                    ))),
                    ")")
                  },
                  "\\right]}^{b_{[1]}/b_{[0]}=",
                  gsub("i", "\\\\jmath",
                       stripImagZero(
                         round(handlesb()[2] / handlesb()[1],
                               max(
                                 c(
                                   2L,
                                   countZeroDigitsRightOfDecimalPoint(handlesb()[2] / handlesb()[1])
                                 )
                               ,na.rm=TRUE))
                       )),
                  "}\\cdot z^{-1}"
                )
              ), if (length(handlesb()) > 2L) {
                switch(
                  abs(sign(Re(
                    handlesb()[3] / handlesb()[1]
                  ))) +
                    1L,
                  if (abs(Im(handlesb()[3] / handlesb()[1])) <=
                      1e-06) {
                    "+(0\\cdot z^{-2})"
                  } else {
                    paste0(
                      "\\cdot (",
                      gsub("i", "\\\\jmath", stripImagZero(
                        round(handles$zeroloc[2],
                              max(
                                c(
                                  2L,
                                  countZeroDigitsRightOfDecimalPoint(handles$zeroloc[i])
                                )
                              ,na.rm=TRUE))
                      )),
                      ")}^{b_{[2]}/b_{[0]}=",
                      gsub("i", "\\\\jmath",
                           stripImagZero(
                             round(handlesb()[3] / handlesb()[1],
                                   max(
                                     c(
                                       2L,
                                       countZeroDigitsRightOfDecimalPoint(handlesb()[2L + 1] /
                                                                            handlesb()[1])
                                     )
                                   ,na.rm=TRUE))
                           )),
                      "}\\cdot z^{-2}"
                    )
                  },
                  paste0(
                    "+ \\overbrace{(",
                    gsub("i", "\\\\jmath",
                         stripImagZero(round(
                           handles$zeroloc[1], max(c(
                             2L,
                             countZeroDigitsRightOfDecimalPoint(handles$zeroloc[1])
                           ),na.rm=TRUE)
                         ))),
                    ")",
                    "\\cdot (",
                    gsub("i", "\\\\jmath", stripImagZero(round(
                      handles$zeroloc[2],
                      max(c(
                        2L,
                        countZeroDigitsRightOfDecimalPoint(handles$zeroloc[i])
                      ),na.rm=TRUE)
                    ))),
                    ")}^{b_{[2]}/b_{[0]}=",
                    gsub("i", "\\\\jmath",
                         stripImagZero(
                           round(handlesb()[3] / handlesb()[1],
                                 max(
                                   c(
                                     2L,
                                     countZeroDigitsRightOfDecimalPoint(handlesb()[i +
                                                                                     1] /
                                                                          handlesb()[1])
                                   )
                                 ,na.rm=TRUE))
                         )),
                    "}\\cdot z^{-2}"
                  )
                )
              })
            },
            "}{",
            "1",
            if (length(handlesa()) > 1L) {
              paste0(switch(
                abs(sign(Re(
                  handles$poleloc[1]
                ))) + 1L,
                if (abs(Im(handles$poleloc[1])) <= 1e-06) {
                  "-(0\\cdot z^{-1})"
                } else {
                  paste0(
                    "\\underbrace{-\\left[(",
                    gsub("i", "\\\\jmath",
                         stripImagZero(round(
                           handles$poleloc[1], max(c(
                             2L,
                             countZeroDigitsRightOfDecimalPoint(handles$poleloc[1])
                           ),na.rm=TRUE)
                         ))),
                    ")",
                    if (length(handles$poleloc) > 1L) {
                      paste0("+(", gsub("i", "\\\\jmath", stripImagZero(
                        round(handles$poleloc[2],
                              max(
                                c(2L,
                                  countZeroDigitsRightOfDecimalPoint(handles$poleloc[2])
                                )
                              ,na.rm=TRUE))
                      )),
                      ")")
                    },
                    "\\right]}_{a_{[1]}=",
                    gsub("i", "\\\\jmath",
                         stripImagZero(round(
                           handlesa()[2], max(c(
                             2L,
                             countZeroDigitsRightOfDecimalPoint(handlesa()[2])
                           ),na.rm=TRUE)
                         ))),
                    "}\\cdot z^{-1}"
                  )
                },
                paste0(
                  "\\underbrace{-\\left[(",
                  gsub("i", "\\\\jmath",
                       stripImagZero(round(
                         handles$poleloc[1], max(c(
                           2L,
                           countZeroDigitsRightOfDecimalPoint(handles$poleloc[1])
                         ),na.rm=TRUE)
                       ))),
                  ")",
                  if (length(handles$poleloc) > 1L) {
                    paste0("+(", gsub("i", "\\\\jmath", stripImagZero(round(
                      handles$poleloc[2],
                      max(c(
                        2L,
                        countZeroDigitsRightOfDecimalPoint(handles$poleloc[2])
                      ),na.rm=TRUE)
                    ))),
                    ")")
                  },
                  "\\right]}_{a_{[1]}=",
                  gsub("i", "\\\\jmath",
                       stripImagZero(round(
                         handlesa()[2], max(c(
                           2L, countZeroDigitsRightOfDecimalPoint(handlesa()[2])
                         ),na.rm=TRUE)
                       ))),
                  "}\\cdot z^{-1}"
                )
              ), if (length(handlesa()) > 2L) {
                switch(
                  abs(sign(Re(
                    handlesa()[3]
                  ))) + 1L,
                  if (abs(Im(handlesa()[3])) <=
                      1e-06) {
                    "-(0\\cdot z^{-2})"
                  } else {
                    paste0(
                      "\\underbrace{+(",
                      gsub("i", "\\\\jmath",
                           stripImagZero(
                             round(handles$poleloc[1], max(
                               c(
                                 2L,
                                 countZeroDigitsRightOfDecimalPoint(handles$poleloc[1])
                               )
                             ,na.rm=TRUE))
                           )),
                      ")",
                      "\\cdot (",
                      gsub("i", "\\\\jmath", stripImagZero(
                        round(handles$poleloc[2],
                              max(
                                c(
                                  2L,
                                  countZeroDigitsRightOfDecimalPoint(handles$poleloc[i])
                                )
                              ,na.rm=TRUE))
                      )),
                      ")}_{a_{[2]}=",
                      gsub("i", "\\\\jmath", stripImagZero(round(
                        handlesa()[3],
                        max(c(
                          2L,
                          countZeroDigitsRightOfDecimalPoint(handlesa()[i +
                                                                          1])
                        ),na.rm=TRUE)
                      ))),
                      "}\\cdot z^{-2}"
                    )
                  },
                  paste0(
                    "\\underbrace{+(",
                    gsub("i", "\\\\jmath",
                         stripImagZero(round(
                           handles$poleloc[1], max(c(
                             2L,
                             countZeroDigitsRightOfDecimalPoint(handles$poleloc[1])
                           ),na.rm=TRUE)
                         ))),
                    ")",
                    "\\cdot (",
                    gsub("i", "\\\\jmath", stripImagZero(round(
                      handles$poleloc[2],
                      max(c(
                        2L,
                        countZeroDigitsRightOfDecimalPoint(handles$poleloc[i])
                      ),na.rm=TRUE)
                    ))),
                    ")}_{a_{[2]}=",
                    gsub("i", "\\\\jmath", stripImagZero(round(
                      handlesa()[3],
                      max(c(
                        2L, countZeroDigitsRightOfDecimalPoint(handlesa()[i +
                                                                           1])
                      ),na.rm=TRUE)
                    ))),
                    "}\\cdot z^{-2}"
                  )
                )
              })
            },
            "}="
          )
        },
        if (input$edit_gain != 1) {
          paste0(round(input$edit_gain, max(
            c(
              5L,
              countZeroDigitsRightOfDecimalPoint(input$edit_gain)
            )
          ,na.rm=TRUE)),
          "\\cdot")
        },
        "\\frac{",
        {
          polyproduct <- 1
          for (i in 1L:length(handles$zeroloc)) {
            polyproduct <-
              pracma::polymul(polyproduct, c(1, handles$zeroloc[i])) # pracma::conv(polyproduct, c(1, handles$zeroloc[i])) # stats::convolve(polyproduct, rev(c(1, handles$zeroloc[i])))
          }
          numeratorstring <-
            if (max(Mod(polyproduct),na.rm=TRUE) > 1e+06) {
              "0"
            }
          else {
            "1"
          }
          if (length(polyproduct) > 1L) {
            for (i in 2L:length(polyproduct)) {
              switch(
                sign(
                  round(Re(polyproduct[i]),3) # rounds "very-small" numbers down to be zero-values
                )
                + 2L,
                numeratorstring <- paste0(numeratorstring, # for negative-values
                                          switch((i %% 2L) + 1L, "+", "-"), 
                                          if (polyproduct[i] == 1) { # isTRUE(all.equal( #
                                                      ""
                                                    } else {
                                                      paste0(if ((abs(Im(
                                                        polyproduct[i]
                                                      )) > 1e-06) ||
                                                      (abs(Re(
                                                        polyproduct[i]
                                                      )) > 1e+05) || (abs(Re(
                                                        polyproduct[i]
                                                      )) <
                                                      1e-05)) {
                                                        "("
                                                      },
                                                      gsub("i", "\\\\jmath", stripImagZero(round(
                                                        if (is.infinite(polyproduct[i])) {
                                                          sign(polyproduct[i])
                                                        } else {
                                                          -polyproduct[i] / if (max(Mod(polyproduct),na.rm=TRUE) > 1e+06) {
                                                            max(Mod(polyproduct),na.rm=TRUE)
                                                          } else {
                                                            1
                                                          }
                                                        }, max(c(
                                                          2,
                                                          countZeroDigitsRightOfDecimalPoint(polyproduct[i])
                                                        ),na.rm=TRUE)
                                                      ))),
                                                      if ((abs(Im(
                                                        polyproduct[i]
                                                      )) > 1e-06) || (abs(Re(
                                                        polyproduct[i]
                                                      )) >
                                                      1e+05) ||
                                                      (abs(Re(
                                                        polyproduct[i]
                                                      )) < 1e-05)) {
                                                        ")"
                                                      },
                                                      "\\cdot ")
                                                    }, "z^{-", i - 1L, "}"),
                "", # for (exactly) Real-zero values
                numeratorstring <- paste0(numeratorstring, # for positive-values
                                          switch((i %% 2L) + 1L, "-", "+"), 
                                          if (polyproduct[i] == -1) { # isTRUE(all.equal( #
                                                      ""
                                                    } else {
                                                      paste0(if ((abs(Im(
                                                        polyproduct[i]
                                                      )) > 1e-06) ||
                                                      (abs(Re(
                                                        polyproduct[i]
                                                      )) > 1e+05) || (abs(Re(
                                                        polyproduct[i]
                                                      )) <
                                                      1e-05)) {
                                                        "("
                                                      },
                                                      gsub("i", "\\\\jmath", stripImagZero(round(
                                                        if (is.infinite(polyproduct[i])) {
                                                          sign(polyproduct[i])
                                                        } else {
                                                          polyproduct[i] / if (max(Mod(polyproduct),na.rm=TRUE) > 1e+06) {
                                                            max(Mod(polyproduct),na.rm=TRUE)
                                                          } else {
                                                            1
                                                          }
                                                        }, max(c(
                                                          2,
                                                          countZeroDigitsRightOfDecimalPoint(polyproduct[i])
                                                        ),na.rm=TRUE)
                                                      ))),
                                                      if ((abs(Im(
                                                        polyproduct[i]
                                                      )) > 1e-06) || (abs(Re(
                                                        polyproduct[i]
                                                      )) >
                                                      1e+05) ||
                                                      (abs(Re(
                                                        polyproduct[i]
                                                      )) < 1e-05)) {
                                                        ")"
                                                      },
                                                      "\\cdot ")
                                                    }, "z^{-", i - 1L, "}")
              )
            }
          }
          numeratorstring
        },
        "}{",
        {
          polyproduct <- 1
          for (i in 1L:length(handles$poleloc)) {
            polyproduct <-
              pracma::polymul(polyproduct, c(1, handles$poleloc[i])) # pracma::conv(polyproduct, c(1, handles$poleloc[i])) # stats::convolve(polyproduct, rev(c(1, handles$poleloc[i])))
          }
          denominatorstring <-
            if (max(Mod(polyproduct),na.rm=TRUE) > 1e+06) {
              "0"
            }
          else {
            "1"
          }
          if (length(polyproduct) > 1L) {
            for (i in 2L:length(polyproduct)) {
              switch(
                sign(
                  round(Re(polyproduct[i]),3) # rounds "very-small" numbers down to be zero-values
                )
                + 2L,
                denominatorstring <- paste0( # for negative-values
                  denominatorstring,
                  switch((i %% 2L) + 1L, 
                         "+", 
                         "-"
                         ),
                  if (polyproduct[i] == 1) { # isTRUE(all.equal( #
                    ""
                  } else {
                    paste0(if ((abs(Im(
                      polyproduct[i]
                    )) > 1e-06) ||
                    (abs(Re(
                      polyproduct[i]
                    )) > 1e+05) || (abs(Re(
                      polyproduct[i]
                    )) <
                    1e-05)) {
                      "("
                    },
                    gsub("i", "\\\\jmath", stripImagZero(round(
                      if (is.infinite(polyproduct[i])) {
                        sign(polyproduct[i])
                      } else {
                        -polyproduct[i] / if (max(Mod(polyproduct),na.rm=TRUE) > 1e+06) {
                          max(Mod(polyproduct),na.rm=TRUE)
                        } else {
                          1
                        }
                      }, max(c(
                        2,
                        countZeroDigitsRightOfDecimalPoint(polyproduct[i])
                      ),na.rm=TRUE)
                    ))),
                    if ((abs(Im(
                      polyproduct[i]
                    )) > 1e-06) || (abs(Re(
                      polyproduct[i]
                    )) >
                    1e+05) ||
                    (abs(Re(
                      polyproduct[i]
                    )) < 1e-05)) {
                      ")"
                    },
                    "\\cdot ")
                  },
                  "z^{-",
                  i - 1L,
                  "}"
                ),
                "", # for (exactly) Real-zero values
                denominatorstring <- paste0( # for positive-values
                  denominatorstring,
                  switch((i %%
                            2L) + 1L, "-", "+"),
                  if (polyproduct[i] == -1) { # isTRUE(all.equal( #
                    ""
                  } else {
                    paste0(if ((abs(Im(
                      polyproduct[i]
                    )) > 1e-06) ||
                    (abs(Re(
                      polyproduct[i]
                    )) > 1e+05) || (abs(Re(
                      polyproduct[i]
                    )) <
                    1e-05)) {
                      "("
                    },
                    gsub("i", "\\\\jmath", stripImagZero(round(
                      if (is.infinite(polyproduct[i])) {
                        sign(polyproduct[i])
                      } else {
                        polyproduct[i] / if (max(Mod(polyproduct),na.rm=TRUE) > 1e+06) {
                          max(Mod(polyproduct),na.rm=TRUE)
                        } else {
                          1
                        }
                      }, max(c(
                        2,
                        countZeroDigitsRightOfDecimalPoint(polyproduct[i])
                      ),na.rm=TRUE)
                    ))),
                    if ((abs(Im(
                      polyproduct[i]
                    )) > 1e-06) || (abs(Re(
                      polyproduct[i]
                    )) >
                    1e+05) ||
                    (abs(Re(
                      polyproduct[i]
                    )) < 1e-05)) {
                      ")"
                    },
                    "\\cdot ")
                  },
                  "z^{-",
                  i - 1L,
                  "}"
                )
              )
            }
          }
          denominatorstring
        },
        "}\\end{aligned}$$"
      ) # end paste0
    ) # end withMathJax
  })

  # Render Difference-Equation ----
  output$diffeqn <- renderUI({
    withMathJax(
      paste0(
        "$$\\begin{aligned}", # \\label{eq:diffeqn}",
        "y_{[n]}=",
        "\\left(",
        "\\sum_{k^{\\prime}=1}^{M}{a_{[k^{\\prime}]}\\cdot y_{[n-k^{\\prime}]}}",
        "\\right)",
        "&+\\left(",
        "\\sum_{k=0}^{N}{b_{[k]} \\cdot x_{[n-k]}}",
        "\\right)",
        "\\\\", # new-line
        "y_{[n]} =",
        {
          accumulatorstring <- NULL
          for (i in 2L:length(handlesa())) {
            if (i > 2L) {
              accumulatorstring <- paste0(accumulatorstring, "+")
            }
            switch(
              abs(sign(Re(
                handlesa()[i]
              ))) + 1L,
              accumulatorstring <- paste0(accumulatorstring,
                                          if (Im(handlesa()[i]) <= 1e-06) {
                                            paste0("(0\\cdot y_{[n-", i - 1L, "]})")
                                          } else {
                                            paste0(
                                              "\\underbrace{(",
                                              gsub("i", "\\\\jmath", stripImagZero(round(
                                                handlesa()[i],
                                                max(c(
                                                  2, countZeroDigitsRightOfDecimalPoint(handlesa()[i])
                                                ),na.rm=TRUE)
                                              ))),
                                              ")}_{a_{[",
                                              i - 1L,
                                              "]}}\\cdot y_{[n-",
                                              i - 1L,
                                              "]}"
                                            )
                                          }),
              accumulatorstring <- paste0(
                accumulatorstring,
                "\\underbrace{(",
                gsub("i", "\\\\jmath", stripImagZero(round(
                  handlesa()[i],
                  max(c(
                    2L, countZeroDigitsRightOfDecimalPoint(handlesa()[i])
                  ),na.rm=TRUE)
                ))),
                ")}_{a_{[",
                i - 1L,
                "]}}\\cdot y_{[n-",
                i - 1L,
                "]}"
              )
            )
          }
          accumulatorstring
        },
        switch(
          abs(sign(Re(1))) + 1L,
          "&+(0\\cdot x_{[n]})",
          paste0(
            "&+\\underbrace{",
            gsub("i", "\\\\jmath", 1),
            "}_{b_{[0]}/b_{[0]}}\\cdot x_{[n]}"
          )
        ),
        {
          accumulatorstring <- NULL
          for (i in 2L:length(handlesb())) {
            switch(
              abs(sign(
                # round(
                Re(
                  handlesb()[i] / handlesb()[1]
                )
                # ,3) # rounds "very-small" numbers down to be zero-values
              )) + 1L,
              accumulatorstring <-
                paste0(accumulatorstring, if (abs(Im(
                  handlesb()[i] / handlesb()[1]
                )) <=
                1e-06) {
                  paste0("+(0\\cdot x_{[n-", i - 1L, "]})")
                } else {
                  paste0(
                    "+ \\underbrace{(",
                    gsub("i", "\\\\jmath",
                         stripImagZero(
                           round(handlesb()[i] / handlesb()[1],
                                 max(
                                   c(
                                     2L,
                                     countZeroDigitsRightOfDecimalPoint(handlesb()[i] / handlesb()[1])
                                   )
                                 ,na.rm=TRUE))
                         )),
                    ")}_{b_{[",
                    i - 1L,
                    "]}/b_{[0]}}\\cdot x_{[n-",
                    i - 1L,
                    "]}"
                  )
                }),
              accumulatorstring <- paste0(
                accumulatorstring,
                "+ \\underbrace{(",
                gsub("i", "\\\\jmath", stripImagZero(
                  round(handlesb()[i] / handlesb()[1],
                        max(
                          c(
                            2L,
                            countZeroDigitsRightOfDecimalPoint(handlesb()[i] / handlesb()[1])
                          )
                        ,na.rm=TRUE))
                )),
                ")}_{b_{[",
                i - 1L,
                "]}/b_{[0]}}\\cdot x_{[n-",
                i - 1L,
                "]}"
              )
            )
          }
          accumulatorstring
        },
        "\\\\", # new-line
        "\\underbrace{}_{a_{[0]}\\equiv 1}y_{[n]}",
        {
          accumulatorstring <- NULL
          for (i in 2L:length(handlesa())) {
            switch(
              abs(sign(
                round(Re(
                  handlesa()[i]
                  ),3) # rounds "very-small" numbers down to be zero-values
              )) + 1L,
              accumulatorstring <- paste0(accumulatorstring, # for (exactly) Real-zero values
                                          if (Im(handlesa()[i]) <= 1e-06) {
                                            ""
                                            # paste0("+(\\underbrace{0}_{a_{[",
                                            #        i - 1L,
                                            #        "]}}\\cdot y_{[n-",
                                            #        i - 1L,
                                            #        "]})")
                                          } else {
                                            paste0(
                                              # "\\underbrace{-",
                                              "-",
                                              "\\underbrace{(",
                                              gsub("i",
                                                   "\\\\jmath", stripImagZero(round(
                                                     handlesa()[i],
                                                     max(c(
                                                       2L, countZeroDigitsRightOfDecimalPoint(handlesa()[i])
                                                     ),na.rm=TRUE)
                                                   ))),
                                              ")}_{a_{[",
                                              i - 1L,
                                              "]}}",
                                              # "]}}}_{-a_{[",
                                              # i - 1L,
                                              # "]}=",
                                              # gsub("i", "\\\\jmath", stripImagZero(round(
                                              #   -handlesa()[i],
                                              #   max(c(
                                              #     2L,
                                              #     countZeroDigitsRightOfDecimalPoint(-handlesa()[i])
                                              #   ),na.rm=TRUE)
                                              # ))),
                                              # "}\\cdot y_{[n-",
                                              "\\cdot y_{[n-",
                                              i - 1L,
                                              "]}"
                                            )
                                          }),
              accumulatorstring <- paste0( # for positive/ negative values
                accumulatorstring,
                # "\\underbrace{-",
                "-",
                "\\underbrace{(",
                gsub("i", "\\\\jmath",
                     stripImagZero(round(
                       handlesa()[i], max(c(
                         2L, countZeroDigitsRightOfDecimalPoint(handlesa()[i])
                       ),na.rm=TRUE)
                     ))),
                ")}_{a_{[",
                i - 1L,
                "]}}",
                # "]}}}_{-a_{[",
                # i - 1L,
                # "]}=",
                # gsub("i",
                #      "\\\\jmath", stripImagZero(round(
                #        -handlesa()[i],
                #        max(c(
                #          2L, countZeroDigitsRightOfDecimalPoint(-handlesa()[i])
                #        ),na.rm=TRUE)
                #      ))),
                # "}\\cdot y_{[n-",
                "\\cdot y_{[n-",
                i - 1L,
                "]}"
              )
            )
          }
          accumulatorstring
        },
        "\\ \\ &=\\ \\ ", # adds some extra spaces
        switch(
          abs(sign(
            round(Re(1L),3) # rounds "very-small" numbers down to be zero-values
          ))
          + 1L,
          "(\\underbrace{0}_{b_{[0]}/b_{[0]}}\\cdot x_{[n]})", # for (exactly) Real-zero values
          paste0( # for positive/ negative values
            "\\underbrace{",
            gsub("i", "\\\\jmath", if (max(Mod(handlesb(
            )),na.rm=TRUE) >
            1e+06) {
              0L
            } else {
              1L
            }),
            "}_{b_{[0]}/b_{[0]}}\\cdot x_{[n]}"
          )
        ),
        {
          accumulatorstring <- NULL
          for (i in 2L:length(handlesb())) {
            switch(
              abs(sign(
                round(Re(
                  handlesb()[i] / handlesb()[1]
                ),3) # rounds "very-small" numbers down to be zero-values
              ))
              + 1L,
              accumulatorstring <- # for (exactly) Real-zero values
                paste0(accumulatorstring, 
                       if (
                         round(
                           Re(handlesb()[i] / handlesb()[1])
                         ,3) # rounds "very-small" numbers down to be zero-values
                         ==0) {
                         ""
                  # paste0("+(\\underbrace{0}_{b_{[",
                  #        i - 1L,
                  #        "]}/b_{[0]}}\\cdot x_{[n-",
                  #        i - 1L,
                  #        "]})")
                } else {
                  paste0(
                    "+ \\underbrace{(",
                    gsub("i", "\\\\jmath",
                         stripImagZero(round(
                           if (max(Mod(handlesb()),na.rm=TRUE) >
                               1e+06) {
                             (handlesb()[i] / handlesb()[1]) / (1/ eps) / 1000
                           } else {
                             handlesb()[i] / handlesb()[1]
                           }, max(c(
                             2L,
                             countZeroDigitsRightOfDecimalPoint(handlesb()[i] / handlesb()[1])
                           ),na.rm=TRUE)
                         ))),
                    ")}_{b_{[",
                    i - 1L,
                    "]}/b_{[0]}}\\cdot x_{[n-",
                    i - 1L,
                    "]}"
                  )
                }),
              accumulatorstring <- paste0( # for positive/ negative values
                accumulatorstring,
                "+ \\underbrace{(",
                gsub("i", "\\\\jmath", stripImagZero(round(
                  if (max(Mod(handlesb()),na.rm=TRUE) >
                      1e+06) {
                    (handlesb()[i] / handlesb()[1]) / (1/ eps) / 1000
                  } else {
                    handlesb()[i] / handlesb()[1]
                  }, max(c(
                    2L,
                    countZeroDigitsRightOfDecimalPoint(handlesb()[i] / handlesb()[1])
                  ),na.rm=TRUE)
                ))),
                ")}_{b_{[",
                i - 1L,
                "]}/b_{[0]}}\\cdot x_{[n-",
                i -
                  1L,
                "]}"
              )
            )
          }
          accumulatorstring
        },
        "\\end{aligned}$$"
      ) # end paste0
    ) # end withMathJax
  })
  
  # output$rect2polarPZplot Text ----
  output$rect2polarPZplot <- renderText({
    shiny::req(input$edit_polezeroloc, input$edit_polezerolocImag)
    if ((grepl("rnorm|runif", input$edit_polezeroloc)) ||
        (grepl("rnorm|runif",
               input$edit_polezerolocImag))) {
      invalidateLater(10000) # update every ~10 seconds
    }
    if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezeroloc,ignore.case=FALSE))) return() # info-system security
    if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocImag,ignore.case=FALSE))) return() # info-system security
    n1 <- try(eval(parse(text = input$edit_polezeroloc)),silent=TRUE)
    n2 <- try(eval(parse(text = input$edit_polezerolocImag)),silent=TRUE)
    if (is.infinite(n2)) {
      a <- Re(n1)
      b <- n2
    }
    else {
      a <- Re(n1 + 1i * n2)
      b <- Im(n1 + 1i * n2)
    }
    Mytheta <- atan2(b, a)
    paste0(
      "radius: ",
      round(sqrt(a * a + b * b), 3L),
      ", ",
      "theta: ",
      round(Mytheta, 3L),
      if (Mytheta != 0) {
        paste0("= ", switch(sign(Mytheta) + 2L, "-", "", ""), "pi",
               "/", round(abs(pi / Mytheta), 1L))
      },
      " rads",
      " (",
      round(Mytheta * 180 / pi, 0L),
      " deg)" # , ^{&bsol;circ}, &bsol;degree, &bsol;deg, &deg;, &bsol;textdegree)" # https://dev.w3.org/html5/html-author/charref
    )
  })
  
  # output$polar2rectPZplot Text ----
  output$polar2rectPZplot <- renderText({
    if ((grepl("rnorm|runif", input$edit_polezerolocRadius)) ||
        (grepl("rnorm|runif",
               input$edit_polezerolocAngle))) {
      invalidateLater(10000)
    }
    if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocRadius,ignore.case=FALSE))) return() # info-system security
    if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocAngle,ignore.case=FALSE))) return() # info-system security
    r <- try(eval(parse(text = input$edit_polezerolocRadius)),silent=TRUE)
    theta <- try(eval(parse(text = input$edit_polezerolocAngle)),silent=TRUE)
    a <- r * exp(1i * theta)
    aReal <- Re(a)
    if (abs(aReal) <= 2 * eps) {
      aReal <- 0
    }
    aImag <- Im(a)
    if (Mod(aImag) <= 2 * eps) {
      aImag <- 0
    }
    paste("Real: ", aReal, "; ", "Imag: ", aImag, "j")
  })
  
  # observeEvent (button) addpole ----
  observeEvent(eventExpr = input$pb_addpole, handlerExpr = {
    if (input$tabPoleZeroEditing == "rtheta") {
      if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocRadius,ignore.case=FALSE))) return() # info-system security
      if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocAngle,ignore.case=FALSE))) return() # info-system security
      n1 <- try(eval(parse(text = input$edit_polezerolocRadius)),silent=TRUE)
      n2 <- try(eval(parse(text = input$edit_polezerolocAngle)),silent=TRUE)
      valueToBeAdded <- n1 * exp(n2)
      if (is.infinite(Im(n2))) {
        valueToBeAdded <- complex(1, n1, switch(sign(n2) + 2L,-Inf,
                                                0L, Inf))
      }
      else if (is.infinite(Re(n2))) {
        valueToBeAdded <- complex(1, n1, switch(sign(n2) + 2L,-Inf,
                                                0L, Inf))
      }
      else {
        if ((is.null(input$edit_currentSelectionText)) ||
            (is.na(input$edit_currentSelectionText)) ||
            (input$edit_currentSelectionText == "")) {
          valueToBeAdded <- n1 * exp(1i * n2)
        }
        else {
          if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_currentSelectionText,ignore.case=FALSE))) return() # info-system security
          valueToBeAdded <-
            try(eval(parse(text = input$edit_currentSelectionText)),silent=TRUE)
        }
      }
      if (Mod(valueToBeAdded) > 1e-06) {
        if (abs(Im(valueToBeAdded)) == 0) { # isTRUE(all.equal( #
          valueToBeAdded <- Re(valueToBeAdded)
        }
        shinyBS::closeAlert(session, alertId = "noMorePoles")
        shinyBS::closeAlert(session, alertId = "noMoreZeros")
        if ((is.null(handles$poleloc)) ||
            (is.na(handles$poleloc)) ||
            ((length(handles$poleloc) == 1L) &&
             (handles$poleloc[1] == 0)) # isTRUE(all.equal( #
            ) {
          handles$poleloc <- valueToBeAdded
        }
        else {
          handles$poleloc <- c(handles$poleloc, valueToBeAdded)
        }
        if (input$tb_addconjugate) {
          handles$poleloc <- c(handles$poleloc, Conj(valueToBeAdded))
        }
        updateSelectInput(session,
                          inputId = "listbox_pole",
                          choices = handles$poleloc)
      }
      else {
        showNotification(
          ui = "Value is too small (or clear the edit-field of the zero-value)...",
          duration = 3,
          closeButton = TRUE,
          type = "message"
        )
      }
    }
    else if (input$tabPoleZeroEditing == "RealImag") {
      if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezeroloc,ignore.case=FALSE))) return() # info-system security
      if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocImag,ignore.case=FALSE))) return() # info-system security
      n1 <- try(eval(parse(text = input$edit_polezeroloc)),silent=TRUE)
      n2 <- try(eval(parse(text = input$edit_polezerolocImag)),silent=TRUE)
      if (is.infinite(Im(n2))) {
        valueToBeAdded <- complex(1, n1, switch(sign(n2) + 2L,-Inf,
                                                0L, Inf))
      }
      else if (is.infinite(Re(n2))) {
        valueToBeAdded <- complex(1, n1, switch(sign(n2) + 2L,-Inf,
                                                0L, Inf))
      }
      else {
        if ((is.null(input$edit_currentSelectionText)) ||
            (is.na(input$edit_currentSelectionText)) ||
            (input$edit_currentSelectionText == "")) {
          valueToBeAdded <- n1 + 1i * n2
        }
        else {
          if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_currentSelectionText,ignore.case=FALSE))) return() # info-system security
          valueToBeAdded <-
            try(eval(parse(text = input$edit_currentSelectionText)),silent=TRUE)
        }
      }
      if ((Mod(valueToBeAdded) > 1e-06)) {
        shinyBS::closeAlert(session, alertId = "noMorePoles")
        shinyBS::closeAlert(session, alertId = "noMoreZeros")
        if (abs(Im(valueToBeAdded)) == 0) { # isTRUE(all.equal( #
          valueToBeAdded <- Re(valueToBeAdded)
        }
        if ((is.null(handles$poleloc)) ||
            (is.na(handles$poleloc)) ||
            ((length(handles$poleloc) == 1L) &&
             (handles$poleloc[1] == 0))
            ) { # isTRUE(all.equal( #
          handles$poleloc <- valueToBeAdded
        }
        else {
          handles$poleloc <- c(handles$poleloc, valueToBeAdded)
        }
        if (input$tb_addconjugate) {
          handles$poleloc <- c(handles$poleloc, Conj(valueToBeAdded))
        }
        updateSelectInput(session,
                          inputId = "listbox_pole",
                          choices = handles$poleloc)
      }
      else {
        showNotification(
          ui = "Value is too small (or clear the edit-field of the zero-value)...",
          duration = 3,
          closeButton = TRUE,
          type = "message"
        )
      }
    }
    else if (input$tabPoleZeroEditing == "CommonFilters") {
      if ((is.null(input$edit_currentSelectionText)) ||
          (is.na(input$edit_currentSelectionText)) ||
          (input$edit_currentSelectionText == "")) {
        inputFilterCommandString <- input$commonFilters
      }
      else {
        inputFilterCommandString <- input$edit_currentSelectionText
      }
      if (grepl("Zpg|sftrans|bilinear", inputFilterCommandString)) {
        if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", inputFilterCommandString,ignore.case=FALSE))) return() # info-system security
        zpg <- try(eval(parse(text = inputFilterCommandString)),silent=TRUE)
        if (is.list(zpg)) {
        handles$zeroloc <- zpg$zero
        handles$poleloc <- zpg$pole
        updateNumericInput(session, inputId = "edit_gain", value = zpg$gain)
        b <- zpg$gain
        length(b) <- 1L
        }
      }
      else if (grepl(
        "fir1|fir2|remez|spencerFilter|sgolayfilt|sgolay|Ma|chebwin|kaiser|bartlett|blackman|boxcar|flattopwin|gausswin|hanning|hamming|triang|sinc",
        inputFilterCommandString
      )) {
        handles$poleloc <- c(0)
        if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", inputFilterCommandString,ignore.case=FALSE))) return() # info-system security
        b <- try(eval(parse(text = inputFilterCommandString)),silent=TRUE)
        if (length(b) < 2L) {
          handles$zeroloc <- c(0)
        }
        else {
          handles$zeroloc <- try(polyroot(rev(b)),silent=TRUE) # numerical stability may be an issue for all but low-degree polynomials
        }
      }
      else if (grepl("FftFilter", inputFilterCommandString)) {
        handles$poleloc <- c(0)
        if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", inputFilterCommandString,ignore.case=FALSE))) return() # info-system security
        b <- try(eval(parse(text = inputFilterCommandString)),silent=TRUE)
        if (length(b) < 2L) {
          handles$zeroloc <- c(0)
        }
        else {
          handles$zeroloc <- try(polyroot(rev(b)),silent=TRUE) # numerical stability may be an issue for all but low-degree polynomials
        }
      }
      else {
        if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", inputFilterCommandString,ignore.case=FALSE))) return() # info-system security
        a <- try(eval(parse(text = paste0(inputFilterCommandString,"$a"))),silent=TRUE)
        if (length(a) < 2L) {
          handles$poleloc <- c(0)
        }
        else {
          handles$poleloc <- try(polyroot(rev(a)),silent=TRUE) # numerical stability may be an issue for all but low-degree polynomials
        }
        if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", inputFilterCommandString,ignore.case=FALSE))) return() # info-system security
        b <- try(eval(parse(text = paste0(inputFilterCommandString,"$b"))),silent=TRUE)
        if (length(b) < 2L) {
          handles$zeroloc <- c(0)
        }
        else {
          handles$zeroloc <- try(polyroot(rev(b)),silent=TRUE) # numerical stability may be an issue for all but low-degree polynomials
        }
      }
      updateSelectInput(session,
                        inputId = "listbox_zero",
                        choices = handles$zeroloc)
      updateSelectInput(session,
                        inputId = "listbox_pole",
                        choices = handles$poleloc)
    }
    else if (input$tabPoleZeroEditing == "Import") {
      inpFile <- input$filecoordsImport
      if (is.null(inpFile))
        return(NULL)
      if (input$textbinaryformatImport == "csv") {
        rawList <- readLines(con = inpFile$name) # inpFile$datapath) # input$filecoordsImport$datapath)
        Npoles <- match("#zerolocs", rawList) - match("#polelocs",
                                                      rawList) - 1L
        importedPoles <- scan(
          file = inpFile$name, # inpFile$datapath, # input$filecoordsImport$datapath,
          n = Npoles,
          quiet = TRUE,
          what = if (input$coordsevaluateImport) {
            character()
          }
          else {
            complex()
          },
          comment.char = "#",
          sep = "\t"
        )
        if (input$coordsevaluateImport) {
          importedPolesValues <- rep(0, length(importedPoles))
          for (i in 1L:length(importedPoles)) {
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", importedPoles[i],ignore.case=FALSE))) return() # info-system security
            importedPolesValues[i] <- try(eval(parse(text = importedPoles[i])),silent=TRUE)
          }
        }
        else {
          importedPolesValues <- importedPoles
        }
        handles$poleloc <- importedPolesValues
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
        importedZeros <-
          scan(
            file = inpFile$name, # input$filecoordsImport$datapath #,
            skip = match("#zerolocs",
                         rawList),
            quiet = TRUE,
            what = if (input$coordsevaluateImport) {
              character()
            }
            else {
              complex()
            },
            comment.char = "#",
            sep = "\t"
          )
        if (input$coordsevaluateImport) {
          importedZerosValues <- rep(0, length(importedZeros))
          for (i in 1L:length(importedZeros)) {
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", importedZeros[i],ignore.case=FALSE))) return() # info-system security
            importedZerosValues[i] <- try(eval(parse(text = importedZeros[i])),silent=TRUE)
          }
        }
        else {
          importedZerosValues <- importedZeros
        }
        handles$zeroloc <- importedZerosValues
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
      }
      else if (input$textbinaryformatImport == "bin") {
        # cat(file=stderr(),"L12066 input$filecoordsImport$datapath:",input$filecoordsImport$datapath,".\n")
        zz <- file(description = inpFile$name, open = "rb") # input$filecoordsImport$datapath, open = "rb") # inpFile$datapath, open = "rb") #
        handles$poleloc <- readBin(
          con = zz,
          what = complex(),
          n = 5L
        )
        dummy <- readBin(con = zz,what = complex(),n = 1L) # zero-value seperator between poles and zeros
        handles$zeroloc <- readBin(
          con = zz,
          what = complex(),
          n = 5L
        )
        print(cbind(Zeros = handles$zeroloc, Poles = handles$poleloc),
                  na.print = "")
        # print(readBin(
        #     con = zz,
        #     what = complex(),
        #     n = 20L # (maximal) number of records to be read
        #   ))
        close(zz)
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
      }
      else if (input$textbinaryformatImport == "RData") {
        # cat(file=stderr(),"L12084 input$filecoordsImport$datapath:",input$filecoordsImport$datapath,".\n")
        load(file = inpFile$name) # input$filecoordsImport$datapath #, verbose = TRUE)
        handles$poleloc <- mypolescopy
        handles$zeroloc <- myzeroscopy
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
      }
      else if (input$textbinaryformatImport == "mat") {
        # cat(file=stderr(),"L12092 input$filecoordsImport$datapath:",input$filecoordsImport$datapath,".\n")
        data <- R.matlab::readMat(con = inpFile$name) # input$filecoordsImport$datapath) # inpFile$datapath) # Returns a named-list structure containing all variables in the MAT file structure
        # handlesb <- data$Num
        # handlesa <- data$Den
        handles$zeroloc <- try(polyroot(rev(data$Num)),silent=TRUE) # i.e. Numerator
        handles$poleloc <- try(polyroot(rev(data$Den)),silent=TRUE) # i.e. Denominator
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
      }
      else if (input$textbinaryformatImport == "yml") {
        # cat(file=stderr(),"L12111 input$filecoordsImport$datapath:",input$filecoordsImport$datapath,".\n")
        data <- yaml::yaml.load_file(input = inpFile$name) # input$filecoordsImport$datapath) # inpFile$datapath)
        # cat(file=stderr(),"L12112 length(data$mypolescopy):",length(data$mypolescopy),".\n")
        # cat(file=stderr(),"L12113 length(data$myzeroscopy):",length(data$myzeroscopy),".\n")
        for (i in (1L:length(data$mypolescopy))) {
          if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", data$mypolescopy[i],ignore.case=FALSE))) return() # info-system security
          handles$poleloc[i] <- try(eval(parse(text = data$mypolescopy[i])),silent=TRUE)
        }
        for (i in (1L:length(data$myzeroscopy))) {
          if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", data$mypolescopy[i],ignore.case=FALSE))) return() # info-system security
          handles$zeroloc[i] <- try(eval(parse(text = data$myzeroscopy[i])),silent=TRUE)
        }
        unlink(input$filecoordsImport$datapath) # inpFile$datapath)
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
      }
    }
  })
  
  # observeEvent (button) addzero ----
  observeEvent(eventExpr = input$pb_addzero, handlerExpr = {
    if (input$tabPoleZeroEditing == "rtheta") {
      if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocRadius,ignore.case=FALSE))) return() # info-system security
      if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocAngle,ignore.case=FALSE))) return() # info-system security
      n1 <- try(eval(parse(text = input$edit_polezerolocRadius)),silent=TRUE)
      n2 <- try(eval(parse(text = input$edit_polezerolocAngle)),silent=TRUE)
      if (n2 > 2 * pi) {
        n2 <- 2 * pi
      }
      if (n2 < -2 * pi) {
        n2 <- -2 * pi
      }
      if ((is.null(input$edit_currentSelectionText)) ||
          (is.na(input$edit_currentSelectionText)) ||
          (input$edit_currentSelectionText == "")) {
        valueToBeAdded <- n1 * exp(1i * n2)
      }
      else {
        if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_currentSelectionText,ignore.case=FALSE))) return() # info-system security
        valueToBeAdded <-
          try(eval(parse(text = input$edit_currentSelectionText)),silent=TRUE)
      }
      if ((!is.null(valueToBeAdded)) && (!is.na(valueToBeAdded)) &&
          (Im(valueToBeAdded) == 0) # isTRUE(all.equal( #
          ) {
        valueToBeAdded <- Re(valueToBeAdded)
      }
      if (valueToBeAdded != 0) {
        shinyBS::closeAlert(session, alertId = "noMoreZeros")
        shinyBS::closeAlert(session, alertId = "noMorePoles")
        if ((is.null(handles$zeroloc)) ||
            (is.na(handles$zeroloc)) ||
            ((length(handles$zeroloc) == 1L) &&
             (handles$zeroloc[1] == 0)) # isTRUE(all.equal( #
            ) {
          handles$zeroloc <- valueToBeAdded
        }
        else {
          handles$zeroloc <- c(handles$zeroloc, valueToBeAdded)
        }
        if (input$tb_addconjugate) {
          handles$zeroloc <- c(handles$zeroloc, Conj(valueToBeAdded))
        }
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
      }
    }
    else if (input$tabPoleZeroEditing == "RealImag") {
      shiny::req(input$edit_polezeroloc, input$edit_polezerolocImag)
      if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezeroloc,ignore.case=FALSE))) return() # info-system security
      if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocImag,ignore.case=FALSE))) return() # info-system security
      n1 <- try(eval(parse(text = input$edit_polezeroloc)),silent=TRUE)
      n2 <- try(eval(parse(text = input$edit_polezerolocImag)),silent=TRUE)
      if (is.infinite(Im(n2))) {
        valueToBeAdded <- complex(1, n1, switch(sign(n2) + 2L,-Inf,
                                                0, Inf))
      }
      else if (is.infinite(Re(n2))) {
        valueToBeAdded <- complex(1, n1, switch(sign(n2) + 2L,-Inf,
                                                0, Inf))
      }
      else {
        if ((is.null(input$edit_currentSelectionText)) ||
            (is.na(input$edit_currentSelectionText)) ||
            (input$edit_currentSelectionText == "")) {
          valueToBeAdded <- n1 + 1i * n2
        }
        else {
          if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_currentSelectionText,ignore.case=FALSE))) return() # info-system security
          valueToBeAdded <-
            try(eval(parse(text = input$edit_currentSelectionText)),silent=TRUE)
        }
      }
      if (Mod(valueToBeAdded) > 1e-06) {
        shinyBS::closeAlert(session, alertId = "noMoreZeros")
        shinyBS::closeAlert(session, alertId = "noMorePoles")
        if (abs(Im(valueToBeAdded)) == 0) { # isTRUE(all.equal( #
          valueToBeAdded <- Re(valueToBeAdded)
        }
        if ((is.null(handles$zeroloc)) ||
            (is.na(handles$zeroloc)) ||
            ((length(handles$zeroloc) == 1L) &&
             (handles$zeroloc[1] == 0)) # isTRUE(all.equal( #
            ) {
          handles$zeroloc <- valueToBeAdded
        }
        else {
          handles$zeroloc <- c(handles$zeroloc, valueToBeAdded)
        }
        if (input$tb_addconjugate) {
          handles$zeroloc <- c(handles$zeroloc, Conj(valueToBeAdded))
        }
        updateSelectInput(session,
                          inputId = "listbox_zero",
                          choices = handles$zeroloc)
      }
      else {
        showNotification(
          ui = "Value is too small (or clear the edit-field of the zero-value)...",
          duration = 3,
          closeButton = TRUE,
          type = "message"
        )
      }
    }
    else if (input$tabPoleZeroEditing == "CommonFilters") {
      if ((is.null(input$edit_currentSelectionText)) ||
          (is.na(input$edit_currentSelectionText)) ||
          (input$edit_currentSelectionText == "")) {
        inputFilterCommandString <- input$commonFilters
      }
      else {
        inputFilterCommandString <- input$edit_currentSelectionText
      }
      if (grepl("Zpg|sftrans|bilinear", inputFilterCommandString)) {
        if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", inputFilterCommandString,ignore.case=FALSE))) return() # info-system security
        zpg <- try(eval(parse(text = inputFilterCommandString)),silent=TRUE)
        if (is.list(zpg)) {
        handles$zeroloc <- zpg$zero
        handles$poleloc <- zpg$pole
        updateNumericInput(session, inputId = "edit_gain", value = zpg$gain)
        b <- zpg$gain
        length(b) <- 1L
        }
      }
      else if (grepl(
        "fir1|fir2|remez|spencerFilter|sgolayfilt|sgolay|Ma|chebwin|kaiser|bartlett|blackman|boxcar|flattopwin|gausswin|hanning|hamming|triang|sinc",
        inputFilterCommandString
      )) {
        handles$poleloc <- c(0)
        if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", inputFilterCommandString,ignore.case=FALSE))) return() # info-system security
        b <- try(eval(parse(text = inputFilterCommandString)),silent=TRUE)
        if (length(b) < 2L) {
          handles$zeroloc <- c(0)
        }
        else {
          handles$zeroloc <- try(polyroot(rev(b)),silent=TRUE) # numerical stability may be an issue for all but low-degree polynomials
        }
      }
      else if (grepl("FftFilter", inputFilterCommandString)) {
        handles$poleloc <- c(0)
        if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", inputFilterCommandString,ignore.case=FALSE))) return() # info-system security
        b <- try(eval(parse(text = inputFilterCommandString)),silent=TRUE)
        if (length(b) < 2L) {
          handles$zeroloc <- c(0)
        }
        else {
          handles$zeroloc <- try(polyroot(rev(b)),silent=TRUE) # numerical stability may be an issue for all but low-degree polynomials
        }
      }
      else {
        if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", inputFilterCommandString,ignore.case=FALSE))) return() # info-system security
        a <- try(eval(parse(text = paste0(inputFilterCommandString, "$a"))),silent=TRUE)
        if (length(a) < 2L) {
          handles$poleloc <- c(0)
        }
        else {
          handles$poleloc <- try(polyroot(rev(a)),silent=TRUE) # numerical stability may be an issue for all but low-degree polynomials
        }
        if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", inputFilterCommandString,ignore.case=FALSE))) return() # info-system security
        b <- try(eval(parse(text = paste0(inputFilterCommandString, "$b"))),silent=TRUE)
        if (length(b) < 2L) {
          handles$zeroloc <- c(0)
        }
        else {
          handles$zeroloc <- try(polyroot(rev(b)),silent=TRUE) # numerical stability may be an issue for all but low-degree polynomials
        }
      }
      updateSelectInput(session,
                        inputId = "listbox_zero",
                        choices = handles$zeroloc)
      updateSelectInput(session,
                        inputId = "listbox_pole",
                        choices = handles$poleloc)
    }
    else if (input$tabPoleZeroEditing == "Import") {
      inpFile <- input$filecoordsImport
      if (is.null(inpFile))
        return(NULL)
      if (input$textbinaryformatImport == "csv") {
        # cat(file=stderr(),"L12289 input$filecoordsImport:",".\n")
        # print(input$filecoordsImport)
        # cat(file=stderr(),"L12290 input$filecoordsImport$datapath:",input$filecoordsImport$datapath,".\n")
        rawList <- readLines(con = inpFile$name) # inpFile$datapath) # input$filecoordsImport$datapath)
        Npoles <- match("#zerolocs", rawList) - match("#polelocs",
                                                      rawList) - 1L
        importedPoles <- scan(
          file = inpFile$name, # inpFile$datapath, # input$filecoordsImport$datapath, #
          n = Npoles,
          quiet = TRUE,
          what = if (input$coordsevaluateImport) {
            character()
          }
          else {
            complex()
          },
          comment.char = "#",
          sep = "\t"
        )
        if (input$coordsevaluateImport) {
          importedPolesValues <- rep(0, length(importedPoles))
          for (i in 1L:length(importedPoles)) {
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", importedPoles[i],ignore.case=FALSE))) return() # info-system security
            importedPolesValues[i] <- try(eval(parse(text = importedPoles[i])),silent=TRUE)
          }
        }
        else {
          importedPolesValues <- importedPoles
        }
        handles$poleloc <- importedPolesValues
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
        importedZeros <-
          scan(
            file = inpFile$name, # input$filecoordsImport$datapath,
            skip = match("#zerolocs",
                         rawList),
            quiet = TRUE,
            what = if (input$coordsevaluateImport) {
              character()
            }
            else {
              complex()
            },
            comment.char = "#",
            sep = "\t"
          )
        if (input$coordsevaluateImport) {
          importedZerosValues <- rep(0, length(importedZeros))
          for (i in 1L:length(importedZeros)) {
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", importedZeros[i],ignore.case=FALSE))) return() # info-system security
            importedZerosValues[i] <- try(eval(parse(text = importedZeros[i])),silent=TRUE)
          }
        }
        else {
          importedZerosValues <- importedZeros
        }
        handles$zeroloc <- importedZerosValues
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
      }
      else if (input$textbinaryformatImport == "bin") {
        # cat(file=stderr(),"L12344 input$filecoordsImport$datapath:",input$filecoordsImport$datapath,".\n") # 
        zz <- file(description = inpFile$name, open = "rb") # input$filecoordsImport$datapath, open = "rb")
        handles$poleloc <- readBin(
          con = zz,
          what = complex(),
          n = 5L # assuming exactly 5 poles
        )
        dummy <- readBin(con = zz,what = complex(),n = 1L) # zero-value seperator between poles and zeros
        handles$zeroloc <- readBin(
          con = zz,
          what = complex(),
          n = 5L # assuming exactly 5 zeros
        )
        print(cbind(Zeros = handles$zeroloc, Poles = handles$poleloc),
                  na.print = "")
        # print(readBin(
        #     con = zz,
        #     what = complex(),
        #     n = 20L # (maximal) number of records to be read
        #   ))
        close(zz)
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
      }
      else if (input$textbinaryformatImport == "RData") {
        # cat(file=stderr(),"L12349 input$filecoordsImport$datapath:",input$filecoordsImport$datapath,".\n") # inpFile$datapath:",inpFile$datapath) # 
        load(file = inpFile$name) # input$filecoordsImport$datapath #, verbose = TRUE) # inpFile$datapath, verbose = TRUE) #
        handles$poleloc <- mypolescopy
        handles$zeroloc <- myzeroscopy
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
      }
      else if (input$textbinaryformatImport == "mat") {
        # cat(file=stderr(),"L12357 input$filecoordsImport$datapath:",input$filecoordsImport$datapath,".\n") # inpFile$datapath:",inpFile$datapath) # 
        data <- R.matlab::readMat(con = inpFile$name) # input$filecoordsImport$datapath) # inpFile$datapath) # Returns a named-list structure containing all variables in the MAT file structure
        handles$zeroloc <- try(polyroot(rev(data$Num)),silent=TRUE) # i.e. Numerator
        handles$poleloc <- try(polyroot(rev(data$Den)),silent=TRUE) # i.e. Denominator
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
      }
      else if (input$textbinaryformatImport == "yml") {
        # cat(file=stderr(),"L12387 input$filecoordsImport$datapath:",input$filecoordsImport$datapath,".\n") # "inpFile$datapath",inpFile$datapath) # 
        data <- yaml::yaml.load_file(input = inpFile$name) # input$filecoordsImport$datapath) # inpFile$datapath)
        # cat(file=stderr(),"L12389 length(data$mypolescopy):",length(data$mypolescopy),".\n")
        # cat(file=stderr(),"L12390 length(data$myzeroscopy):",length(data$myzeroscopy),".\n")
        for (i in (1L:length(data$mypolescopy))) {
          if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", data$mypolescopy[i],ignore.case=FALSE))) return() # info-system security
          handles$poleloc[i] <- try(eval(parse(text = data$mypolescopy[i])),silent=TRUE)
        }
        length(handles$poleloc) <- length(data$mypolescopy)
        for (i in (1L:length(data$myzeroscopy))) {
          if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", data$myzeroscopy[i],ignore.case=FALSE))) return() # info-system security
          handles$zeroloc[i] <- try(eval(parse(text = data$myzeroscopy[i])),silent=TRUE)
        }
        length(handles$zeroloc) <- length(data$myzeroscopy)
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
      }
      unlink(input$filecoordsImport$datapath) # inpFile$datapath)
    }
  })
  observeEvent(
    eventExpr = c(
      input$edit_polezeroloc,
      input$edit_polezerolocImag,
      input$edit_polezerolocRadius,
      input$edit_polezerolocAngle
    ),
    handlerExpr = {
      updateTextInput(session, inputId = "edit_currentSelectionText",
                      value = "")
    }
  )
  observeEvent(eventExpr = input$listbox_pole, handlerExpr = {
    if ((input$tabPoleZeroEditing == "RealImag") ||
        (input$tabPoleZeroEditing == "rtheta")
        ) {
      shinyjs::enable(id = "pb_editpole")
      shinyjs::enable(id = "pb_editzero")
      shinyjs::enable(id = "pb_delpole")
      shinyjs::enable(id = "pb_delzero")
    }
    if (input$tabPoleZeroEditing == "RealImag") {
      updateTextInput(session,
                      inputId = "edit_currentSelectionText",
                      value = input$listbox_pole)
    }
    else if (input$tabPoleZeroEditing == "rtheta") {
      if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$listbox_pole,ignore.case=FALSE))) return() # info-system security
      evaluatedinputlistbox_pole <- try(eval(parse(text = input$listbox_pole)),silent=TRUE)
      updateTextInput(
        session,
        inputId = "edit_currentSelectionText",
        value = paste0(
          Mod(evaluatedinputlistbox_pole),
          "*exp(",
          MASS::fractions(Arg(evaluatedinputlistbox_pole) /
                            pi, max.denominator = 13),
          "*pi*j)"
        )
      )
    }
    else if ((input$tabPoleZeroEditing == "CommonFilters") ||
             (input$tabPoleZeroEditing == "Import") ||
             (input$tabPoleZeroEditing == "Export")
             ) {
      updateTabsetPanel(session, inputId = "tabPoleZeroEditing",
                        selected = "RealImag")
      updateTextInput(session,
                      inputId = "edit_currentSelectionText",
                      value = input$listbox_pole)
    }
  })
  observeEvent(eventExpr = input$listbox_zero, handlerExpr = {
    if ((input$tabPoleZeroEditing == "RealImag") ||
        (input$tabPoleZeroEditing == "rtheta")
        ) {
      shinyjs::enable(id = "pb_editpole")
      shinyjs::enable(id = "pb_editzero")
      shinyjs::enable(id = "pb_delpole")
      shinyjs::enable(id = "pb_delzero")
    }
    if (input$tabPoleZeroEditing == "RealImag") {
      updateTextInput(session,
                      inputId = "edit_currentSelectionText",
                      value = input$listbox_zero)
    }
    else if (input$tabPoleZeroEditing == "rtheta") {
      if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$listbox_zero,ignore.case=FALSE))) return() # info-system security
      evaluatedinputlistbox_zero <- try(eval(parse(text = input$listbox_zero)),silent=TRUE)
      updateTextInput(
        session,
        inputId = "edit_currentSelectionText",
        value = paste0(
          Mod(evaluatedinputlistbox_zero),
          "*exp(",
          MASS::fractions(Arg(evaluatedinputlistbox_zero) /
                            pi, max.denominator = 13),
          "*pi*j)"
        )
      )
    }
    else if ((input$tabPoleZeroEditing == "CommonFilters") ||
             (input$tabPoleZeroEditing == "Import") ||
             (input$tabPoleZeroEditing == "Export")) {
      updateTabsetPanel(session, inputId = "tabPoleZeroEditing",
                        selected = "RealImag")
      updateTextInput(session,
                      inputId = "edit_currentSelectionText",
                      value = input$listbox_zero)
    }
  })
  
  # observeEvent (button) editpole ----
  observeEvent(eventExpr = input$pb_editpole, handlerExpr = {
    if ((!is.null(handles$poleloc)) && (!is.na(handles$poleloc))) {
      if ((is.null(input$listbox_pole)) || (is.na(input$listbox_pole))) {
        tgt <- handles$poleloc[length(handles$poleloc)]
      }
      else {
        tgt <- input$listbox_pole
      }
      matchpoint <- match(tgt, handles$poleloc, nomatch = -1)
      if ((matchpoint > 0) &&
          (matchpoint <= length(handles$poleloc))) {
        if (input$tabPoleZeroEditing == "rtheta") {
          if ((is.null(input$edit_currentSelectionText)) ||
              (is.na(input$edit_currentSelectionText)) ||
              (input$edit_currentSelectionText == "")) {
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocRadius,ignore.case=FALSE))) return() # info-system security
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocAngle,ignore.case=FALSE))) return() # info-system security
            valueToReplace <-
              try(eval(parse(text = input$edit_polezerolocRadius)) *
              exp(1i * eval(parse(
                text = input$edit_polezerolocAngle
              ))),silent=TRUE)
          }
          else {
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_currentSelectionText,ignore.case=FALSE))) return() # info-system security
            valueToReplace <-
              try(eval(parse(text = input$edit_currentSelectionText)),silent=TRUE)
          }
        }
        else {
          if ((is.null(input$edit_currentSelectionText)) ||
              (is.na(input$edit_currentSelectionText)) ||
              (input$edit_currentSelectionText == "")) {
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezeroloc,ignore.case=FALSE))) return() # info-system security
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocImag,ignore.case=FALSE))) return() # info-system security
            n1 <- try(eval(parse(text = input$edit_polezeroloc)),silent=TRUE)
            n2 <- try(eval(parse(text = input$edit_polezerolocImag)),silent=TRUE)
            if (is.infinite(Im(n2))) {
              valueToReplace <- complex(1, n1, switch(sign(n2) +
                                                        2L,-Inf, 0, Inf))
            }
            else if (is.infinite(Re(n2))) {
              valueToReplace <- complex(1, n1, switch(sign(n2) +
                                                        2L,-Inf, 0, Inf))
            }
            else {
              valueToReplace <- n1 + 1i * n2
            }
          }
          else {
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_currentSelectionText,ignore.case=FALSE))) return() # info-system security
            valueToReplace <-
              try(eval(parse(text = input$edit_currentSelectionText)),silent=TRUE)
          }
        }
      }
      if ((Mod(valueToReplace) > 1e-06)) {
        if (abs(Im(valueToReplace)) == 0) { # isTRUE(all.equal( #
          valueToReplace <- Re(valueToReplace)
        }
        handles$poleloc[matchpoint] <- valueToReplace
        updateSelectInput(session,
                          inputId = "listbox_pole",
                          choices = handles$poleloc)
      }
      else {
        showNotification(
          ui = "Value is too small (or clear the edit-field of the zero-value)...",
          duration = 3,
          closeButton = TRUE,
          type = "message"
        )
      }
    }
  })
  
  # observeEvent (button) editzero ----
  observeEvent(eventExpr = input$pb_editzero, handlerExpr = {
    if ((!is.null(handles$zeroloc)) && (!is.na(handles$zeroloc))) {
      if ((is.null(input$listbox_zero)) || (is.na(input$listbox_zero))) {
        tgt <- handles$zeroloc[length(handles$zeroloc)]
      }
      else {
        tgt <- input$listbox_zero
      }
      matchpoint <- match(tgt, handles$zeroloc, nomatch = -1)
      if ((matchpoint > 0) &&
          (matchpoint <= length(handles$zeroloc))) {
        if (input$tabPoleZeroEditing == "rtheta") {
          if ((is.null(input$edit_currentSelectionText)) ||
              (is.na(input$edit_currentSelectionText)) ||
              (input$edit_currentSelectionText == "")) {
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocRadius,ignore.case=FALSE))) return() # info-system security
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocAngle,ignore.case=FALSE))) return() # info-system security
            valueToReplace <-
              try(eval(parse(text = input$edit_polezerolocRadius)) *
              exp(1i * eval(parse(
                text = input$edit_polezerolocAngle
              ))),silent=TRUE)
          }
          else {
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_currentSelectionText,ignore.case=FALSE))) return() # info-system security
            valueToReplace <-
              try(eval(parse(text = input$edit_currentSelectionText)),silent=TRUE)
          }
        }
        else {
          if ((is.null(input$edit_currentSelectionText)) ||
              (is.na(input$edit_currentSelectionText)) ||
              (input$edit_currentSelectionText == "")) {
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezeroloc,ignore.case=FALSE))) return() # info-system security
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_polezerolocImag,ignore.case=FALSE))) return() # info-system security
            n1 <- try(eval(parse(text = input$edit_polezeroloc)),silent=TRUE)
            n2 <- try(eval(parse(text = input$edit_polezerolocImag)),silent=TRUE)
            if (is.infinite(Im(n2))) {
              valueToReplace <- complex(1, n1, switch(sign(n2) +
                                                        2L,-Inf, 0, Inf))
            }
            else if (is.infinite(Re(n2))) {
              valueToReplace <- complex(1, n1, switch(sign(n2) +
                                                        2L,-Inf, 0, Inf))
            }
            else {
              valueToReplace <- n1 + 1i * n2
            }
          }
          else {
            if (any(grepl("system|shell|pipe|url|file|socket|fifo|open|Sys[.]", input$edit_currentSelectionText,ignore.case=FALSE))) return() # info-system security
            valueToReplace <-
              try(eval(parse(text = input$edit_currentSelectionText)),silent=TRUE)
          }
        }
      }
      if ((Mod(valueToReplace) > 1e-06)) {
        if (abs(Im(valueToReplace)) == 0) { # isTRUE(all.equal( #
          valueToReplace <- Re(valueToReplace)
        }
        handles$zeroloc[matchpoint] <- valueToReplace
        updateSelectInput(session,
                          inputId = "listbox_zero",
                          choices = handles$zeroloc)
      }
      else {
        showNotification(
          ui = "Value is too small (or clear the edit-field of the zero-value)...",
          duration = 3,
          closeButton = TRUE,
          type = "message"
        )
      }
    }
  })
  
  # observeEvent (button) delpole ----
  observeEvent(eventExpr = input$pb_delpole, handlerExpr = {
    if ((is.null(handles$poleloc)) ||
        (is.na(handles$poleloc)) || ((length(handles$poleloc) == 1L) &&
                                     (handles$poleloc[1] == 0)) # isTRUE(all.equal( #
        ) {
      shinyBS::createAlert(
        session,
        anchorId = "AlertMsgToUser",
        alertId = "noMorePoles",
        title = "Oops",
        style = "info",
        content = "Nothing in poles-list, currently...",
        dismiss = TRUE,
        append = FALSE
      )
      showNotification(
        ui = "Nothing in poles-list, currently...",
        duration = 3,
        closeButton = TRUE,
        type = "message"
      )
    }
    else {
      if ((is.null(input$listbox_pole)) || (is.na(input$listbox_pole))) {
        tgt <- handles$poleloc[length(handles$poleloc)]
      }
      else {
        tgt <- input$listbox_pole
      }
      if (length(handles$poleloc) == 1L) {
        handles$poleloc <- NA
      }
      else {
        matchpoint <- match(tgt, handles$poleloc, nomatch = -1)
        str(matchpoint)
        temppoleloc <- NULL
        if ((matchpoint > 0) &&
            (matchpoint < length(handles$poleloc))) {
          temppoleloc <-
            c(handles$poleloc[(matchpoint + 1L):length(handles$poleloc)])
        }
        if (matchpoint > 1L) {
          temppoleloc <- c(handles$poleloc[1L:(matchpoint - 1L)],
                           temppoleloc)
        }
        handles$poleloc <- temppoleloc
      }
      if (is.null(handles$poleloc) || is.na(handles$poleloc)) {
        updateSelectInput(session,
                          "listbox_pole",
                          choices = 0,
                          selected = 0)
        handles$poleloc <- c(0)
      }
      else {
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
      }
    }
  })
  
  # observeEvent (button) delzero ----
  observeEvent(eventExpr = input$pb_delzero, handlerExpr = {
    if ((is.null(handles$zeroloc)) ||
        (is.na(handles$zeroloc)) || ((length(handles$zeroloc) == 1L) &&
                                     (handles$zeroloc[1] == 0)) # isTRUE(all.equal( #
        ) {
      shinyBS::createAlert(
        session,
        anchorId = "AlertMsgToUser",
        alertId = "noMoreZeros",
        title = "Oops",
        style = "info",
        content = "Nothing in zeros-list, currently...",
        dismiss = TRUE,
        append = FALSE
      )
      showNotification(
        ui = "Nothing in zeros-list, currently...",
        duration = 3,
        closeButton = TRUE,
        type = "message"
      )
    }
    else {
      if ((is.null(input$listbox_zero)) || (is.na(input$listbox_zero))) {
        tgt <- handles$zeroloc[length(handles$zeroloc)]
      }
      else {
        tgt <- input$listbox_zero
      }
      if (length(handles$zeroloc) == 1L) { # isTRUE(all.equal( #
        handles$zeroloc <- NA
      }
      else {
        matchpoint <- match(tgt, handles$zeroloc, nomatch = -1)
        tempzeroloc <- NULL
        if ((matchpoint > 0L) &&
            (matchpoint < length(handles$zeroloc))) {
          tempzeroloc <-
            c(handles$zeroloc[(matchpoint + 1L):length(handles$zeroloc)])
        }
        if (matchpoint > 1L) {
          tempzeroloc <- c(handles$zeroloc[1L:(matchpoint - 1L)],
                           tempzeroloc)
        }
        handles$zeroloc <- tempzeroloc
      }
      if (is.null(handles$zeroloc) || is.na(handles$zeroloc)) {
        updateSelectInput(
          session,
          inputId = "listbox_zero",
          choices = 0,
          selected = 0
        )
        handles$zeroloc <- c(0)
      }
      else {
        updateSelectInput(session,
                          inputId = "listbox_zero",
                          choices = handles$zeroloc)
      }
    }
  })
  output$pixelratio <- renderUI({
    shiny::withMathJax(
      shiny::HTML(
        round(100 * session$clientData$pixelratio),
        "%",
        "is the current pixel-ratio (zoom-level) of your web-browser",
        "(current browser-dimensions: ",
        input$dimension[1],
        "w \\(\\times\\)",
        input$dimension[2],
        "h",
        "; initial 'screen':",
        input$GetScreenWidth,
        "w \\(\\times\\)",
        input$GetScreenHeight,
        "h",
        ")",
        "(typically, can use CTRL-plus/ CTRL-minus, and/or CTRL-mousewheel, to change this)"
      )
    )
  })
})

enableBookmarking(store = "url")

Myapp123 <- shinyApp(ui = ui, server = server)
