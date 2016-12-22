options(shiny.sanitize.errors = TRUE) # http://shiny.rstudio.com/articles/sanitize-errors.html
# options(rgl.useNULL = TRUE)
memory.limit(size = 1e+05)

# ADD-ON PACKAGES **************************************************** ----
library(shiny)
library(shinyBS) # bsModal, bsButton, bsCollapse/ bsCollapsePanel, updateButton, createAlert/ closeAlert
library(shinyjs) # reset, info, delay, show, hide, disable, onevent, hidden
library(signal) # fir1, fir2, butter, cheby1, cheby2, ellip, butterord, cheby1ord, ellipord, remez, spencerFilter, sgolay, bilinear, sftrans, freqz, unwrap, filter, fftfilt, impz, bartlett, blackman, boxcar, chebwin, flattopwin, gausswin, hanning, hamming, triang, Zpg, Arma, Ma, sgolay
library(pracma) # zeros, fliplr/ flipud, ifft, Toeplitz, inv, isempty, polar, meshgrid, polyval, fftshift, Poly, polymul, str2num, findpeaks, linspace
library(MASS) # fractions
library(colourpicker) # colourInput
#library(colorspace) # rainbow_hcl, diverge_hcl
#library(RColorBrewer) # brewer.pal
library(threejs) # scatterplotThreeOutput/ renderScatterplotThree, scatterplot3js
library(rgl) # rglwidgetOutput, renderRglwidget, persp3d, plot3d, surface3d, rglwidget, playwidget, scene3d, rgl.close, clear3d
# library(knitr) # include_graphics; Note: automatic calculation of the output width requires the png package (for PNG images)
library(R.matlab) # readMat, writeMat
library(yaml) # yaml.load_file, as.yaml
library(rmarkdown) # render, pandoc_available
library(markdown) # markdownToHTML
library(devtools) # session_info
library(png)
library(webshot) # appshot

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
scalePlotsToVerticalHeight <- TRUE
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
    "yeti"
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
MyChosenTheme <- sample(AvailableThemes, 1)
MyAppNameAndTheme <- paste(MyChosenTheme, "<b>P</b>e<b>Z</b>")
mySidebarWidth <- 5

# FUNCTIONS ********************************************************** ----
sinc <- function(x) {
  ifelse(x == 0, 1, sin(pi * x) / (pi * x))
}

stripImagZero <- function(x) {
  if (max(abs(Im(x))) < .Machine$double.eps ^ 0.5) {
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
      return(0)
    }
    else if (abs(Im(x)) < tol) {
      x <- abs(Re(x))
      if (((x - floor(x)) < tol) || abs(x) < tol) {
        return(0)
      }
      y <- -log10(x - floor(x))
      return(floor(y) - (y %% 1 < tol))
    }
    else {
      xReal <- abs(Re(x))
      if (((xReal - floor(xReal)) < tol) || (xReal < tol)) {
        return(0)
      }
      yReal <- -log10(xReal - floor(xReal))
      xImag <- abs(Im(x))
      if (((xImag - floor(xImag)) < tol) || (xImag < tol)) {
        return(0)
      }
      yImag <- -log10(xImag - floor(xImag))
      return(min(4, max(c(
        floor(yReal) - (yReal %% 1 < tol),
        floor(yImag) -
          (yImag %% 1 < tol)
      ), na.rm = TRUE), na.rm = TRUE))
    }
  }

unWrap <- function(p, tol = (pi - eps)) {
  # https://www.dsprelated.com/freebooks/sasp/Matlab_listing_unwrap_m.html
  N <- length(p)
  up <- rep(0, times = N)
  pm1 <- p[1]
  up[1] <- pm1
  po <- 0
  thr <- pi - eps
  twopi <- 2 * pi
  for (i in 2:N) {
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
                    N = 1024,
                    whole = 1,
                    fs = 1) {
  # https://ccrma.stanford.edu/~jos/fp/Frequency_Response_Matlab.html
  # "Introduction to Digital Filters with Audio Applications", Julius O. Smith III, (Sept/2007)
  na <- length(A)
  nb <- length(B)
  if ((is.complex(B)) || (is.complex(A))) {
    whole <- 1
  }
  Nf <- 2 * N
  if (whole == 1) {
    Nf <- N
  }
  w <- (2 * pi * fs * (0:(Nf - 1)) / Nf)
  H <-
    fft(c(B, pracma::zeros(1, Nf - nb))) / fft(c(A, pracma::zeros(1,
                                                                  Nf - na)))
  if (whole == 1) {
    w <- w[1:N]
    H <- H[1:N]
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
  abline(h = 0, col = "black")
  abline(v = 0, col = "black")
  plot(
    c(0:(N - 1)) * fs / N,
    Arg(H),
    type = "l",
    col = "blue",
    xlab = flab,
    ylab = "Phase"
  )
  grid(col = "grey")
  abline(h = 0, col = "black")
  abline(v = 0, col = "black")
  return(list(H = H, w = w))
}

myfindpeaks <-
  function(x,
           nups = 1,
           ndowns = nups,
           zero = "0",
           peakpat = NULL,
           minpeakheight = -Inf,
           minpeakdistance = 1,
           threshold = 0,
           npeaks = 0,
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
    if (rc[1] < 0)
      return(NULL)
    x1 <- rc
    x2 <- rc + attr(rc, "match.length")
    attributes(x1) <- NULL
    attributes(x2) <- NULL
    n <- length(x1)
    xv <- xp <- numeric(n)
    for (i in 1:n) {
      xp[i] <- which.max(x[x1[i]:x2[i]]) + x1[i] - 1
      xv[i] <- x[xp[i]]
    }
    inds <-
      which(xv >= minpeakheight & xv - pmax(x[x1], x[x2]) >= threshold)
    X <- cbind(xv[inds], xp[inds], x1[inds], x2[inds])
    if (minpeakdistance < 1)
      warning("Handling 'minpeakdistance < 1' is logically not possible.")
    if (sortstr || minpeakdistance > 1) {
      sl <- sort.list(X[, 1], na.last = NA, decreasing = TRUE)
      X <- X[sl, , drop = FALSE]
    }
    if (length(X) == 0)
      return(c())
    if (minpeakdistance > 1) {
      no_peaks <- nrow(X)
      badpeaks <- rep(FALSE, no_peaks)
      for (i in 1:no_peaks) {
        ipos <- X[i, 2]
        if (!badpeaks[i]) {
          dpos <- abs(ipos - X[, 2])
          badpeaks <- badpeaks | (dpos > 0 & dpos < minpeakdistance)
        }
      }
      X <- X[!badpeaks,]
    }
    if (is.vector(X)) { # <<< different from original `pracma` package's function-code
      if (npeaks > 0 && npeaks < length(X) / 4) { # <<< different from original `pracma` package's function-code
        X <- X[1:npeaks, , drop = FALSE]
      }
    }
    else {
      if (npeaks > 0 && npeaks < nrow(X)) { # <<< different from original `pracma` package's function-code
        X <- X[1:npeaks, , drop = FALSE]
      }
    }
    return(X)
  }

stabilityCheck <- function(A) {
  # https://www.dsprelated.com/freebooks/filters/Pole_Zero_Analysis_I.html
  N <- length(A) - 1
  stable <- 1
  for (i in seq(N, 1, by = -1)) {
    rci <- A[i + 1]
    if (Mod(rci) >= 1) {
      stable <- 0
      break
    }
    A <- (A[1:i] - rci * A[seq((i + 1), 2, by = -1)]) / (1 - rci ^ 2)
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
    x <- 0:(pt - 1)
    tempi <- Im(handleshn)
    xx <- matrix(0, nrow = 1, ncol = (3 * pt))
    xx[seq(from = 1, to = 3 * pt, by = 3)] <- x
    xx[seq(from = 2, to = 3 * pt, by = 3)] <- x
    xx[seq(from = 3, to = 3 * pt, by = 3)] <- NaN
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
      abline(h = 0)
      abline(v = 0)
      output$system_real <- renderUI({
        tags$span(style = paste0("color:", input$ForegroundColor),
                  "real-valued")
      })
    }
    else {
      yy <- matrix(0, nrow = 1, ncol = 3 * pt)
      yy[seq(from = 2,
             to = 3 * pt,
             by = 3)] <- tempi
      yy[seq(from = 3,
             to = 3 * pt,
             by = 3)] <- NaN
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
      abline(h = 0)
      abline(v = 0)
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
      #     2 / 3,
      #   lty = "dashed"
      # )
      output$system_real <- renderUI({
        tags$span(style = "color:magenta",
                  "[has imaginary-components (Note: not plotted here)]")
      })
    }
    yy <- matrix(0, nrow = 1, ncol = 3 * pt)
    yy[seq(from = 2, to = 3 * pt, by = 3)] <- Re(handleshn)
    yy[seq(from = 3, to = 3 * pt, by = 3)] <- NaN
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
      lwd = input$LineWidth * 2 / 3,
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
    x <- 0:(pt - 1)
    tempi <- Im(handleshnu)
    xx <- matrix(0, nrow = 1, ncol = (3 * pt))
    xx[seq(from = 1, to = 3 * pt, by = 3)] <- x
    xx[seq(from = 2, to = 3 * pt, by = 3)] <- x
    xx[seq(from = 3, to = 3 * pt, by = 3)] <- NaN
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
      abline(h = 0)
      abline(v = 0)
      text(pt-1,Re(handleshnu[pt]),labels=round(Re(handleshnu[pt]),3),pos=4) # steady-state value
      # output$system_real <- renderUI({
      #   tags$span(style = paste0("color:", input$ForegroundColor),
      #             "real-valued")
      # })
    # }
    # else {
    #   yy <- matrix(0, nrow = 1, ncol = 3 * pt)
    #   yy[seq(from = 2,
    #          to = 3 * pt,
    #          by = 3)] <- tempi
    #   yy[seq(from = 3,
    #          to = 3 * pt,
    #          by = 3)] <- NaN
    #   plot(
    #     x,
    #     tempi,
    #     type = "p",
    #     pch = 21,
    #     bg = input$BackgroundColor,
    #     xlim = c(0, pt),
    #     ylim = c(min(c(-1, yy, tempi), na.rm = TRUE) *
    #                1.2, max(c(1, yy, tempi), na.rm = TRUE) * 1.2),
    #     col = "magenta",
    #     lwd = input$LineWidth,
    #     xlab = "n",
    #     ylab = "h[n]",
    #     main = "Unit-Step Response"
    #   )
    #   grid(col = input$grcolor)
    #   abline(h = 0)
    #   abline(v = 0)
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
    #       2 / 3,
    #     lty = "dashed"
    #   )
    #   output$system_real <- renderUI({
    #     tags$span(style = "color:magenta",
    #               "[has imaginary-components (Note: not properly plotted here)]")
    #   })
    # }
    yy <- matrix(0, nrow = 1, ncol = 3 * pt)
    yy[seq(from = 2, to = 3 * pt, by = 3)] <- Re(handleshnu)
    yy[seq(from = 3, to = 3 * pt, by = 3)] <- NaN
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
      lwd = input$LineWidth * 2 / 3,
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
        title = div(HTML(MyAppNameAndTheme),
                    img(src = "bigorb.png", height = 20
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
          icon = shiny::icon("bar-chart-o", lib = "font-awesome"), # http://fontawesome.io/icons/
          shinyBS::bsModal(
            id = "modalExample",
            title = "Your plot",
            trigger = "pb_showgph",
            size = "large",
            plotOutput("plotshowgph"),
            downloadButton(outputId = "downloadShowgphPlot", label = "Download")
          ),
          fluidPage(title = "PeZDemo Plots Page", fluidRow(
            # . sidebarLayout ----
            sidebarLayout(
              position = "left",
              fluid = TRUE,
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
                      width = 6,
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
                      width = 6,
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
                              6,
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
                            ), column(6, tags$span(
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
                                    height = "600px",
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
                                      min = -5,
                                      max = 5,
                                      value = c(-1,
                                                1),
                                      step = 0.25,
                                      ticks = TRUE,
                                      animate = FALSE,
                                      sep = " ",
                                      dragRange = TRUE
                                    ),
                                    sliderInput(
                                      inputId = "zoomlimY",
                                      label = NULL,
                                      min = -5,
                                      max = 5,
                                      value = c(-1,
                                                1),
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
                            if ((scalePlotsToVerticalHeight)) {
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
                              height = "600px",
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
                                          width = 4,
                                          numericInput(
                                            inputId = "nticks3D",
                                            label = "N ticks",
                                            value = 6,
                                            min = 1,
                                            max = 20,
                                            step = 1
                                          )
                                        )),
                              tags$span(title = "tooltip: either a single hex or named color name (all points same color), or a vector of #' hex or named color names as long as the number of data points to plot.",
                                        column(
                                          width = 4,
                                          numericInput(
                                            inputId = "colors3D",
                                            label = "N colors",
                                            value = 32,
                                            min = 1,
                                            max = 64,
                                            step = 1
                                          )
                                        )),
                              conditionalPanel(
                                condition = "(input.renderer3D == 'canvas') || (input.renderer3D == 'auto')",
                                tags$span(title = "tooltip: (only supported by the 'canvas'-type renderer)",
                                          column(
                                            width = 4,
                                            numericInput(
                                              inputId = "sizes3D",
                                              label = "Point-size",
                                              value = 0.1,
                                              min = 0.1,
                                              max = 6,
                                              step = 0.1
                                            )
                                          ))
                              )
                            ),
                            fluidRow(
                              tags$span(title = "tooltip: The 'canvas' renderer is the fallback rendering option when 'webgl' is not available. Select 'auto' to automatically choose between the two. The two renderers produce slightly different-looking output and have different available options (see above). Use the 'webgl' renderer for plotting large numbers of points (if available). Use the 'canvas' renderer to excercise finer-control of plotting of smaller numbers of points.",
                                        column(
                                          width = 4,
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
                                          width = 4, checkboxInput("usegrid3D",
                                                                   label = "Grid", value = FALSE)
                                        )),
                              tags$span(title = "tooltip: display of a plane at z=0\nBetter, but slower, in 'canvas' mode",
                                        column(
                                          width = 4,
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
                                      width = 6,
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
                                              0,
                                              "(1-eps)*exp(-2i*pi/3)",
                                              "(1-8*eps)*exp(-2i*pi/3)",
                                              0.5,
                                              "1-eps",
                                              "1/sqrt(2)",
                                              "1-0.5*3.276i",
                                              "0.2*j+0.4",
                                              "0.475+sqrt(3)/2*0.95*1i",
                                              "rnorm(1,mean=0,sd=0.5)",
                                              "rnorm(1,mean=0,sd=0.5)+rnorm(1,mean=0,sd=0.5)*1i",
                                              "runif(1,min=-0.999,max=0.999)",
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
                                      width = 6,
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
                                              0,
                                              0.5,
                                              "1/sqrt(2)",
                                              "cos(pi/3)",
                                              "sinpi(0.5)",
                                              "1-eps",
                                              "rnorm(1,mean=0,sd=0.5)",
                                              "runif(1,min=-0.999,max=0.999)",
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
                                      width = 6,
                                      tags$span(
                                        title = "tooltip: enter numbers/ R-language equations, or pull-down for previous entries",
                                        selectizeInput(
                                          inputId = "edit_polezerolocRadius",
                                          label = HTML("<b>&rarrbfs;</b> (radius)"),
                                          choices = c(
                                            0,
                                            0.4,
                                            "1-eps",
                                            "1/sqrt(2)",
                                            "sqrt(3)/2",
                                            "abs(rnorm(1,mean=0,sd=0.5))",
                                            "runif(1,min=0,max=0.999)",
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
                                      width = 6,
                                      title = "enter numbers/ R-language equations, or pull-down for previous entries",
                                      tags$span(
                                        title = "tooltip: enter numbers/ R-language equations, or pull-down for previous entries",
                                        selectizeInput(
                                          inputId = "edit_polezerolocAngle",
                                          label = HTML("<b>&angmsdaa;</b> (angle)"),
                                          choices = c(
                                            "0",
                                            "pi",
                                            "0.25*pi",
                                            "37.2*pi/180",
                                            "runif(1,min=0,max=1)*2*pi",
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
                              `FIR Window, order 10, BPF, 0.3 to 0.7, Hamming window` = "N=10;fir1(n=N,w=c(0.3,0.7),type=\"pass\",window=hamming(N+1),scale=TRUE)",
                              `FIR Window, order 10, BSF, 0.3 to 0.7, Hamming window` = "N=10;fir1(n=N,w=c(0.3,0.7),type=\"stop\",window=hamming(N+1),scale=TRUE)",
                              `FIR, arbitrary piecewise-linear (type II), order 100, BPF, 0.3 to 0.7, Hamming` = "fir2(n=100, f=c(0, 0.3, 0.3, 0.7, 0.7, 1), m=c(0, 0, 1, 0.5, 0, 0), grid_n=512, ramp_n=5, window=hamming(101))",
                              `Arbitrary MA, given b` = "Ma(b=c(1/3,2/3,1/3))",
                              `Arbitrary ARMA, given b,a` = "Arma(b=c(1/3,2/3,1/3), a=c(1,1-eps))",
                              `Arbitrary ARMA, given poles,zeros` = "Zpg(zero=c(-1,-1), pole=c(-(1-eps)), gain=1/3)",
                              `Zero located at infinity, given b` = "Ma(b=c(1,-fpmaxx))",
                              `Over-damped: real Poles` = "Zpg(zero=c(0), pole=c(-0.92345,0.92345), gain=1)",
                              `Under-damped (damped-sinusoid, exponential envelope): complex-conjugate Poles` = "Zpg(zero=c(0), pole=c(-0.5+0.52345i,-0.5-0.52345i), gain=1)",
                              `Un-damped (oscillating, natural-frequency, resonates): imaginary Poles` = "Zpg(zero=c(0), pole=c(0.92345i,-0.92345i), gain=1)",
                              `Critical-damped: multiple/ repeated/ co-located real Poles` = "Zpg(zero=c(0), pole=c(-0.92345,-0.92345), gain=1)",
                              `LPF, Bilinear z-Transform, conversion of analog-prototype, cutoff=0.4` = "omegac=0.4;omegacprime=tan(omegac*pi/2);Zpg(zero=c(-1), pole=c(-(omegacprime-1)/(omegacprime+1)), gain=omegacprime/(omegacprime+1))",
                              `LPF, Bilinear z-Transform (function), conversion of analog, cutoff=0.4` = "omegac=0.4;signal::bilinear(Sz=c(-fpmaxx/10),Sp=c(-tan(pi*omegac/2)),Sg=tan(pi*omegac/2)/(fpmaxx/10),T=2)",
                              `Transform band-edges of s-plane LPF 0.3 to a z-plane BPF 0.3-0.7` = "omegac1=0.3;omegac2=0.7;signal::sftrans(Sz=c(-1e16),Sp=c(-tan(pi*omegac1/2)),Sg=tan(pi*omegac1/2)/(1e16),W=c(omegac1,omegac2),stop=FALSE)",
                              `Chebyshev I, order 5, 3dB ripple, LPF, 0.3` = "cheby1(n=5,Rp=3,W=0.3,type=\"low\")",
                              `Chebyshev I, order 5, 3dB ripple, HPF, 0.7` = "cheby1(n=5,Rp=3,W=0.7,type=\"high\")",
                              `Chebyshev I, order 5, 3dB ripple, BPF, 0.3 to 0.7` = "cheby1(n=5,Rp=3,W=c(0.3,0.7),type=\"pass\")",
                              `Chebyshev I, order 5, 3dB ripple, BSF, 0.3 to 0.7` = "cheby1(n=5,Rp=3,W=c(0.3,0.7),type=\"stop\")",
                              `Sinusoidal/ Oscillator/ Resonator, 2 poles on imag-axis, near unit-circle` = "Arma(b=c(1), a=c(1,0,(1-eps)))",
                              `Sinusoidal/ Oscillator/ Resonator, 2 conjugate-poles, arbitrary angle` = "theta=1/6;Zpg(zero=c(0), pole=0.99999999999999*c(exp(theta*pi*1i),exp(-theta*pi*1i)), gain=1)",
                              `L-point Moving-Average, L=5, FIR` = "FftFilter(rep(1/5,times=5),n=512)$b",
                              `Three-term (Delay-Line) 'IIR-equivalent' Moving-Average filter, N=5` = "N=5;Arma(b=c(1/N,rep(0,times=N-1),-1/N),a=c(1,-(1-eps)))",
                              `Cascaded Integrator-Comb CIC/ Hogenauer (MovAvg filter), ratio R=5 (b,a)` = "R=5;M=1;Arma(b=c(1,rep(0,times=R*M-1),-1), a=c(1,-(1-eps)))",
                              `Cascaded Integrator-Comb CIC/ Hogenauer (MovAvg filter), ratio R=8 (Zpg)` = "N=8;Zpg(zero=c(1,-1,1i,-1i,1/sqrt(2)+1/sqrt(2)*1i,1/sqrt(2)-1/sqrt(2)*1i,-1/sqrt(2)+1/sqrt(2)*1i,-1/sqrt(2)-1/sqrt(2)*1i), pole=c(1-eps), gain=1/N)",
                              `Comb-Filter, 5 poles w/3 zeros` = "Arma(b=c(1,0,0, 0.5^3), a=c(1,0,0,0,0, 0.9^5))",
                              `Integrator 1/s, given b,a; pole at +1, zero at -1` = "Arma(b=c(1,1), a=c(1,-(1-eps)))",
                              `pole at -1, zero at +1`= "Zpg(zero=c(1), pole=c(-(1-eps)), gain=1)",
                              `Notch-Filter, Fractional-Sample Delay-line, D=2pi/omega0, (Pei Tseng '98 Fig 2)` = "omega0=0.22*pi;D=2*pi/omega0;rho=0.99;Arma(b=c(1,rep(0,times=floor(D-1)),-1), a=c(1,rep(0,times=floor(D-1)),-(rho)^D))",
                              `Peaking-Filter, fc=0.22` = "theta=0.22;rp=0.999;rz=0.997;Zpg(zero=c(rz*exp(theta*pi*1i),rz*exp(-theta*pi*1i)), pole=c(rp*exp(theta*pi*1i),rp*exp(-theta*pi*1i)), gain=1)",
                              `Notch-Out Filter, fc=0.22` = "theta=0.22;rp=0.997;rz=0.999;Zpg(zero=c(rz*exp(theta*pi*1i),rz*exp(-theta*pi*1i)), pole=c(rp*exp(theta*pi*1i),rp*exp(-theta*pi*1i)), gain=1)",
                              `Freq-Sampling IIR algorithm (unstable), ord. 12, fc=0.7, delay=7` = "N=12;D=7;fc=0.7;L=2*N;FF=matrix(data=0,nrow=N,ncol=1);for (k in seq(1,N,by=1)){f=2*k/(L+1);if (f <= fc) {FF[k]=exp(-1i*D*f*pi)}};Fb=Conj(FF);FF=c(1,FF,pracma::flipud(Fb));h=Re(pracma::ifft(FF));r1=t(h);r=c(r1[1],pracma::fliplr(as.matrix(t(r1[2:length(r1)]))));H=pracma::Toeplitz(h,r);H0=H[,1:(N+1)];H1=H0[1:(N+1),];h1=H0[seq((N+2),(L+1)),1];H2=H0[seq((N+2),(L+1)),seq(2,(N+1))];ah=-pracma::inv(H2) %*% h1;a=c(1,ah);b=H1 %*% a;list(a=a,b=b)",
                              `Least-Squares IIR algorithm (unstable), ord. 12, fc=0.7, delay=7, 120 Samples` = "N=12;D=7;L=120;fc=0.7;L1=0.5*L;FF=matrix(data=0,nrow=L1,ncol=1);for (k in seq(1,L1)){f=2*k/(L+1);if (f <= fc) {FF[k]=exp(-1i*D*f*pi)}};Fb=Conj(FF);FF=c(1,FF,pracma::flipud(Fb));h=Re(pracma::ifft(FF));r1=t(h);r=c(r1[1],pracma::fliplr(as.matrix(t(r1[2:length(r1)]))));H=pracma::Toeplitz(h,r);H0=H[,1:(N+1)];H1=H0[1:(N+1),];h1=H0[seq((N+2),(L+1)),1];H2=H0[seq((N+2),(L+1)),seq(2,(N+1))];ah=-pracma::inv(t(H2) %*% H2) %*% t(H2) %*% h1;a=c(1,ah);b=H1 %*% a;list(a=a,b=b)",
                              `All-Pass, poles within circle, zeros outside at conjugate-reciprocal` = "r1=1.3;th1=0.6;Zpg(zero=c(r1*exp(th1*pi*1i),r1*exp(-th1*pi*1i)), pole=c((1/r1)*exp(th1*pi*1i),(1/r1)*exp(-th1*pi*1i)), gain=1)",
                              `All-Pass, pole at 0, zero at infinity` = "Arma(b=c(1,-fpmaxx), a=c(1))",
                              `All-Pass, reversed-ordering of (real) filter-coefficients` = "myb=c(1,2,3,4,5,6);Arma(b=myb, a=rev(myb))",
                              `Min. Phase, Delay < 2 (Oppenheim Schafer Buck, 1989, Fig 5.30a)` = "Zpg(zero=c(0.9*exp(0.6*pi*1i),0.9*exp(-0.6*pi*1i),0.8*exp(0.8*pi*1i),0.8*exp(-0.8*pi*1i)), pole=c(0), gain=1)",
                              `Max. Phase, Delay < 12 (Oppenheim Schafer Buck, 1989, Fig 5.30b)` = "Zpg(zero=c(1/0.9*exp(0.6*pi*1i),1/0.9*exp(-0.6*pi*1i),1/0.8*exp(0.8*pi*1i),1/0.8*exp(-0.8*pi*1i)), pole=c(0), gain=1)",
                              `Hilbert (absolute values), order 40, Hamming window` = "N=41;M=20;hz=matrix(data=0,nrow=1,ncol=M);zw=seq(1,(M-1),by=2);hz[seq(1,(M-1),by=2)]=2 / (pi*zw);hd=c(-pracma::fliplr(as.matrix(hz)),0,hz);w=signal::hamming(N);hd*w",
                              `Ideal Differentiator (noiseless inputs only; else, corruption), 23pt Hamming,fs=512,fc=0.3` = "t=seq(0,2-1/512,by=1/512);fs=512;Ts=1/fs;N=23;M=(N-1)/2;n=1:M;h=cos(n*pi)/(Ts*n);h=c(-pracma::fliplr(as.matrix(t(h))),0,h);win=signal::hamming(N);win*h",
                              `Differentiator, Band-Limited, 23-pt Hamming window, fs=512, fc=0.3` = "fs=512;Ts=1/fs;N1=23;M=(N1-1)/2;n=0:(M-1);k=M-n;k2=k^2;fc=0.3*pi;h1=sin(k*fc);h2=(fc*k)*cos(k*fc);hd=(h1-h2)/(Ts*pi*k2);hd=c(hd,0,-pracma::fliplr(as.matrix(t(hd))));win=signal::hamming(N1);win*hd",
                              `Butterworth, order 5, LPF, 0.3` = "butter(n=5,W=0.3,type=\"low\")",
                              `Butterworth, order 5, HPF, 0.7` = "butter(n=5,W=0.7,type=\"high\")",
                              `Butterworth, order 5, BPF, 0.3 to 0.7` = "butter(n=5,W=c(0.3,0.7),type=\"pass\")",
                              `Butterworth, order 5, BSF, 0.3 to 0.7` = "butter(n=5,W=c(0.3,0.7),type=\"stop\")",
                              `Chebyshev II, order 5, 3 dB ripple, LPF, 0.3` = "cheby2(n=5,Rp=20,W=0.3,type=\"low\")",
                              `Chebyshev II, order 5, 3 dB ripple, HPF, 0.7` = "cheby2(n=5,Rp=20,W=0.7,type=\"high\")",
                              `Chebyshev II, order 5, 3 dB ripple, BPF, 0.3 to 0.7` = "cheby2(n=5,Rp=20,W=c(0.3,0.7),type=\"pass\")",
                              `Chebyshev II, order 5, 3 dB ripple, BSF, 0.3 to 0.7` = "cheby2(n=5,Rp=20,W=c(0.3,0.7),type=\"stop\")",
                              `Elliptical, order 5, ripple: 3dB, (40dB stopband), LPF, 0.3` = "ellip(n=5,Rp=3,Rs=40,W=0.3,type=\"low\")",
                              `Elliptical, order 5, ripple: 3dB, (40dB stopband), HPF, 0.7` = "ellip(n=5,Rp=3,Rs=40,W=0.7,type=\"high\")",
                              `Elliptical, order 5, ripple: 3dB, (40dB stopband), BPF, 0.3 to 0.7` = "ellip(n=5,Rp=3,Rs=40,W=c(0.3,0.7),type=\"pass\")",
                              `Elliptical, order 5, ripple: 3dB, (40dB stopband), BSF, 0.3 to 0.7` = "ellip(n=5,Rp=3,Rs=40,W=c(0.3,0.7),type=\"stop\")",
                              `Min. Order, Butterworth: ripple: 0.5dB (29dB stopband), LPF, 0.3` = "butter(n=buttord(Wp=0.285, Ws=0.345, Rp=0.5, Rs=29))",
                              `Min. Order, Chebyshev I: ripple: 0.5dB (29dB stopband), LPF, 0.3` = "cheby1(n=cheb1ord(Wp=0.3, Ws=0.34, Rp=0.5, Rs=29))",
                              `Min. Order, Elliptical: ripple: 0.5dB (29dB stopband), LPF, 0.3` = "ellip(n=ellipord(Wp=0.3, Ws=0.34, Rp=0.5, Rs=29))",
                              `Remez (Parks-McClellan optimal/ equiripple/ minimax FIR), order 15, LPF, 0.3` = "remez(n=15, f= c(0, 0.3, 0.4, 1), a= c(1,1, 0,0), ftype= \"bandpass\")",
                              `Remez, ord. 30, symmetrical FIR (J. Dobes, 2003 Ex1)`="firstHalfCoef=c(0.034025544,0.006219216,-0.005305575,0.006128687,-0.005593423,0.00624262,-0.006848848,0.008979105,-0.008978654,0.017501073,-0.006953636,0.039774499,-0.064655981,0.085240952,-0.131292156);Ma(b=c(firstHalfCoef,0.195140968,rev(firstHalfCoef)))",
                              `Remez, ord. 48, symmetrical FIR (J. Dobes, 2003 Ex2)`="firstHalfCoef=c(0.00012511398639,0.00001335284427,0.00016015250121,0.00000634686622,0.00026201837991,0.00007281852105,0.00045629795460,0.00022819555936,0.00071588589103,0.00047316021190,0.00110189764986,0.00088563032407,0.00184819117706,0.00188050116629,0.00282392666400,0.00363976768981,0.00591155524557,0.00644303257612,0.01406190034797,0.00537162176461,0.03594691432517,0.06164502638211,0.08276620944465,0.13009560635626);Ma(b=c(firstHalfCoef,0.19452719610477,rev(firstHalfCoef)))",
                              `Remez, ord. 48, psychoacoustic/ physiological volume-control FIR (Dobes, 2003 Ex2)`="firstHalfZeros=c(-1.27851808211318+0.62684824819101i,-1.36614299238589+0.27717114439577i,-1.07284006705681+0.91235620647926i,-0.77267104757162+1.12541933041583i,-0.44961436355736+1.22363585379352i,-0.20330452525073+1.29600355151311i,0.14750702212683+1.34841369533889i,0.50634734941036+1.25979827310114i,0.83329614995319+1.04982507756824i,1.04560310563666+0.67293837999034i,1.20771535897979+0.24059979456907i,0.95139216401989+0.56315210274529i,0.77837030003487+0.46073626392613i,0.79640192035334+0.15865836018951i,0.67627030631208+0.43523995090672i,0.46384030886663+0.58436750039291i,0.27466939387352+0.68338074343342i,0.08016782642482+0.73284236586261i,-0.11813451163119+0.75307102211071i,-0.26456653442537+0.72002392155667i,-0.41461446573694+0.60389882069075i,-0.70304843505842+0.14263861132902i,-0.63057376627246+0.30916579614562i,-0.54091513103870+0.46000078868751i);Zpg(zero=c(firstHalfZeros,Conj(firstHalfZeros)),pole=c(0),gain=0.00012511398639)",
                              `Irregular IIR by Chained-Fractions (unstable) (J. Dobes, 2003 Ex3)`="Zpg(zero=c(0.9049098+0.1414979i,0.9049098-0.1414979i,1.192327), pole=c(0.9588639+0.7240575i,0.9588639-0.7240575i,1.511628), gain=1)",
                              `Savitzky-Golay smoothing-filter, order 3 (cubic), length 5` = "p=3;n=5;sgolay(p,n) %*% c(1,rep(0,times=n-1))",
                              `Dolph-Chebyshev window, 50-point, 100dB attenuation` = "chebwin(n=50, at=100)",
                              `Kaiser-window, 101-point, beta 0 (very-wide=Rectangle)` = "kaiser(n=101, beta=0)",
                              `Kaiser-window, 101-point, beta 50 (narrower)` = "kaiser(n=101, beta=50)",
                              `LPF, 0.3, Kaiser-window` = "with(kaiserord(f=c(0.275,0.325), m=c(1,0), dev=c(0.1,0.1)),fir1(n=n,w=Wc,type=type,window=kaiser(n+1,beta),scale=FALSE))",
                              `Rectangle, 101-point, given b` = "Ma(b=rep(1,times=101))",
                              `Bartlett-window, 41-point` = "bartlett(41)",
                              `Blackman-window, 41-point` = "blackman(41)",
                              `Boxcar-window (aka. Rectangular, Dirichlet), 41-point` = "boxcar(41)",
                              `Flattop-window, symmetric, 41-point` = "flattopwin(41,sym=\"symmetric\")",
                              `Flattop-window, periodic (DFT Even), 41-point` = "flattopwin(41,sym=\"periodic\")",
                              `Gaussian-window, 41-point` = "gausswin(41,5)",
                              `vonHann(ing)-window (raised-cosine, sine-squared), 41-point` = "hanning(41)",
                              `Hamming-window, 41-point` = "hamming(41)",
                              `Triangle-window (Bartlett, but no zero-endpoint), 41-point` = "triang(41)",
                              `Windowed-Sinc (e.g. using Blackman), 19-point, 0.3` = "N=18;leftside=sin(pi*(0.3*(-(N/2):(-1))))/(pi*(0.3*(-(N/2):(-1))));c(leftside,1,rev(leftside))*blackman(N+1)", # "N=18;sinc(0.3*(-(N/2):(N/2)))*0.3*blackman(N+1)",
                              `Spencer 15-point Moving-Average Filter` = "spencerFilter()",
                              `Spencer MA, given b` = "Ma(b=c(-3, -6, -5, 3, 21, 46, 67, 74, 67, 46, 21, 3, -5, -6, -3) / 320)",
                              `( random-filter from this list )` = paste0(
                                "sample(c('",
                                paste(
                                  "omegac=0.4;omegacprime=tan(omegac*pi/2);Zpg(zero=c(-1), pole=c(-(omegacprime-1)/(omegacprime+1)), gain=omegacprime/(omegacprime+1))",
                                  "omegac=0.4;signal::bilinear(Sz=c(-fpmaxx/10),Sp=c(-tan(pi*omegac/2)),Sg=tan(pi*omegac/2)/(fpmaxx/10),T=2)",
                                  "omegac1=0.3;omegac2=0.7;signal::sftrans(Sz=c(-1e16),Sp=c(-tan(pi*omegac1/2)),Sg=tan(pi*omegac1/2)/(1e16),W=c(omegac1,omegac2),stop=FALSE)",
                                  "cheby1(n=5,Rp=3,W=0.3,type=\"low\")",
                                  "cheby1(n=5,Rp=3,W=0.7,type=\"high\")",
                                  "cheby1(n=5,Rp=3,W=c(0.3,0.7),type=\"pass\")",
                                  "cheby1(n=5,Rp=3,W=c(0.3,0.7),type=\"stop\")",
                                  "Arma(b=c(1), a=c(1,0,(1-eps)))",
                                  "theta=2/3;Zpg(zero=c(0), pole=c((1-eps)*exp(theta*pi*1i),(1-eps)*exp(-theta*pi*1i)), gain=1)",
                                  "fir1(n=40,w=0.3,type=\"low\",window=hamming(41),scale=TRUE)",
                                  "fir1(n=10,w=c(0.3,0.7),type=\"pass\",window=hamming(11),scale=TRUE)",
                                  "fir1(n=10,w=c(0.3,0.7),type=\"stop\",window=hamming(11),scale=TRUE)",
                                  "fir2(n=100, f=c(0, 0.3, 0.3, 0.7, 0.7, 1), m=c(0, 0, 1, 0.5, 0, 0), grid_n=512, ramp_n=5, window=hamming(101))",
                                  "FftFilter(rep(1/5,times=5),n=512)$b",
                                  "N=5;Arma(b=c(1/N,rep(0,times=N-1),-1/N),a=c(1,-(1-eps)))",
                                  "R=5;M=1;Arma(b=c(1,rep(0,times=R*M-1),-1), a=c(1,-(1-eps)))",
                                  "N=8;Zpg(zero=c(1,-1,1i,-1i,1/sqrt(2)+1/sqrt(2)*1i,1/sqrt(2)-1/sqrt(2)*1i,-1/sqrt(2)+1/sqrt(2)*1i,-1/sqrt(2)-1/sqrt(2)*1i), pole=c(1-eps), gain=1/N)",
                                  "Arma(b=c(1,0,0, 0.5^3), a=c(1,0,0,0,0, 0.9^5))",
                                  "Arma(b=c(1,1), a=c(1,-(1-eps)))",
                                  "N=12;D=7;fc=0.7;L=2*N;FF=matrix(data=0,nrow=N,ncol=1);for (k in seq(1,N,by=1)){f=2*k/(L+1);if (f <= fc) {FF[k]=exp(-1i*D*f*pi)}};Fb=Conj(FF);FF=c(1,FF,pracma::flipud(Fb));h=Re(pracma::ifft(FF));r1=t(h);r=c(r1[1],pracma::fliplr(as.matrix(t(r1[2:length(r1)]))));H=pracma::Toeplitz(h,r);H0=H[,1:(N+1)];H1=H0[1:(N+1),];h1=H0[seq((N+2),(L+1)),1];H2=H0[seq((N+2),(L+1)),seq(2,(N+1))];ah=-pracma::inv(H2) %*% h1;a=c(1,ah);b=H1 %*% a;list(a=a,b=b)",
                                  "N=12;D=7;L=120;fc=0.7;L1=0.5*L;FF=matrix(data=0,nrow=L1,ncol=1);for (k in seq(1,L1)){f=2*k/(L+1);if (f <= fc) {FF[k]=exp(-1i*D*f*pi)}};Fb=Conj(FF);FF=c(1,FF,pracma::flipud(Fb));h=Re(pracma::ifft(FF));r1=t(h);r=c(r1[1],pracma::fliplr(as.matrix(t(r1[2:length(r1)]))));H=pracma::Toeplitz(h,r);H0=H[,1:(N+1)];H1=H0[1:(N+1),];h1=H0[seq((N+2),(L+1)),1];H2=H0[seq((N+2),(L+1)),seq(2,(N+1))];ah=-pracma::inv(t(H2) %*% H2) %*% t(H2) %*% h1;a=c(1,ah);b=H1 %*% a;list(a=a,b=b)",
                                  "Ma(b=c(1/3,2/3,1/3))",
                                  "Arma(b=c(1/3,2/3,1/3), a=c(1,1-eps))",
                                  "Zpg(zero=c(-1,-1), pole=c(-(1-eps)), gain=1/3)",
                                  "Ma(b=c(1,-fpmaxx))",
                                  "R=8;M=1;Arma(b=c(1,rep(0,times=R*M-1),-1), a=c(1,-(1-eps)))",
                                  "r1=1.3;th1=0.6;Zpg(zero=c(r1*exp(th1*pi*1i),r1*exp(-th1*pi*1i)), pole=c((1/r1)*exp(th1*pi*1i),(1/r1)*exp(-th1*pi*1i)), gain=1)",
                                  "Arma(b=c(1,-fpmaxx), a=c(1))",
                                  "myb=c(1,2,3,4,5,6);Arma(b=myb, a=rev(myb))",
                                  "Zpg(zero=c(0.9*exp(0.6*pi*1i),0.9*exp(-0.6*pi*1i),0.8*exp(0.8*pi*1i),0.8*exp(-0.8*pi*1i)), pole=c(0), gain=1)",
                                  "Zpg(zero=c(1/0.9*exp(0.6*pi*1i),1/0.9*exp(-0.6*pi*1i),1/0.8*exp(0.8*pi*1i),1/0.8*exp(-0.8*pi*1i)), pole=c(0), gain=1)",
                                  "t=seq(0,2-1/512,by=1/512);fs=512;Ts=1/fs;N=23;M=(N-1)/2;n=1:M;h=cos(n*pi)/(Ts*n);h=c(-pracma::fliplr(as.matrix(t(h))),0,h);win=signal::hamming(N);win*h",
                                  "N=41;M=20;hz=matrix(data=0,nrow=1,ncol=M);zw=seq(1,(M-1),by=2);hz[seq(1,(M-1),by=2)]=2 / (pi*zw);hd=c(-pracma::fliplr(as.matrix(hz)),0,hz);w=signal::hamming(N);hd*w",
                                  "fs=512;Ts=1/fs;N1=23;M=(N1-1)/2;n=seq(0,M-1,by=1);k=M-n;k2=k^2;fc=0.3*pi;h1=sin(k*fc);h2=(fc*k)*cos(k*fc);hd=(h1-h2)/(Ts*pi*k2);hd=c(hd,0,-pracma::fliplr(as.matrix(t(hd))));win=signal::hamming(N1);h_bld=win*hd",
                                  "theta=0.22;rp=0.999;rz=0.997;Zpg(zero=c(rz*exp(theta*pi*1i),rz*exp(-theta*pi*1i)), pole=c(rp*exp(theta*pi*1i),rp*exp(-theta*pi*1i)), gain=1)",
                                  "theta=0.22;rp=0.997;rz=0.999;Zpg(zero=c(rz*exp(theta*pi*1i),rz*exp(-theta*pi*1i)), pole=c(rp*exp(theta*pi*1i),rp*exp(-theta*pi*1i)), gain=1)",
                                  "omega0=0.22*pi;D=2*pi/omega0;rho=0.99;Arma(b=c(1,rep(0,times=floor(D-1)),-1), a=c(1,rep(0,times=floor(D-1)),-(rho)^D))",
                                  "theta=0.22;rp=0.997;rz=0.999;Zpg(zero=c(rz*exp(theta*pi*1i),rz*exp(-theta*pi*1i)), pole=c(rp*exp(theta*pi*1i),rp*exp(-theta*pi*1i)), gain=1)",
                                  "theta=0.22;rp=0.999;rz=0.997;Zpg(zero=c(rz*exp(theta*pi*1i),rz*exp(-theta*pi*1i)), pole=c(rp*exp(theta*pi*1i),rp*exp(-theta*pi*1i)), gain=1)",
                                  "butter(n=5,W=0.3,type=\"low\")",
                                  "butter(n=5,W=0.7,type=\"high\")",
                                  "butter(n=5,W=c(0.3,0.7),type=\"pass\")",
                                  "butter(n=5,W=c(0.3,0.7),type=\"stop\")",
                                  "cheby2(n=5,Rp=20,W=0.3,type=\"low\")",
                                  "cheby2(n=5,Rp=20,W=0.7,type=\"high\")",
                                  "cheby2(n=5,Rp=20,W=c(0.3,0.7),type=\"pass\")",
                                  "cheby2(n=5,Rp=20,W=c(0.3,0.7),type=\"stop\")",
                                  "ellip(n=5,Rp=3,Rs=40,W=0.3,type=\"low\")",
                                  "ellip(n=5,Rp=3,Rs=40,W=0.7,type=\"high\")",
                                  "ellip(n=5,Rp=3,Rs=40,W=c(0.3,0.7),type=\"pass\")",
                                  "ellip(n=5,Rp=3,Rs=40,W=c(0.3,0.7),type=\"stop\")",
                                  "butter(n=buttord(Wp=0.285, Ws=0.345, Rp=0.5, Rs=29))",
                                  "cheby1(n=cheb1ord(Wp=0.3, Ws=0.34, Rp=0.5, Rs=29))",
                                  "ellip(n=ellipord(Wp=0.3, Ws=0.34, Rp=0.5, Rs=29))",
                                  "remez(n=15, f= c(0, 0.3, 0.4, 1), a= c(1,1, 0,0), ftype= \"bandpass\")",
                                  "firstHalfCoef=c(0.034025544,0.006219216,-0.005305575,0.006128687,-0.005593423,0.00624262,-0.006848848,0.008979105,-0.008978654,0.017501073,-0.006953636,0.039774499,-0.064655981,0.085240952,-0.131292156);Ma(b=c(firstHalfCoef,0.195140968,rev(firstHalfCoef)))",
                                  "firstHalfCoef=c(0.00012511398639,0.00001335284427,0.00016015250121,0.00000634686622,0.00026201837991,0.00007281852105,0.00045629795460,0.00022819555936,0.00071588589103,0.00047316021190,0.00110189764986,0.00088563032407,0.00184819117706,0.00188050116629,0.00282392666400,0.00363976768981,0.00591155524557,0.00644303257612,0.01406190034797,0.00537162176461,0.03594691432517,0.06164502638211,0.08276620944465,0.13009560635626);Ma(b=c(firstHalfCoef,0.19452719610477,rev(firstHalfCoef)))",
                                  "firstHalfZeros=c(-1.27851808211318+0.62684824819101i,-1.36614299238589+0.27717114439577i,-1.07284006705681+0.91235620647926i,-0.77267104757162+1.12541933041583i,-0.44961436355736+1.22363585379352i,-0.20330452525073+1.29600355151311i,0.14750702212683+1.34841369533889i,0.50634734941036+1.25979827310114i,0.83329614995319+1.04982507756824i,1.04560310563666+0.67293837999034i,1.20771535897979+0.24059979456907i,0.95139216401989+0.56315210274529i,0.77837030003487+0.46073626392613i,0.79640192035334+0.15865836018951i,0.67627030631208+0.43523995090672i,0.46384030886663+0.58436750039291i,0.27466939387352+0.68338074343342i,0.08016782642482+0.73284236586261i,-0.11813451163119+0.75307102211071i,-0.26456653442537+0.72002392155667i,-0.41461446573694+0.60389882069075i,-0.70304843505842+0.14263861132902i,-0.63057376627246+0.30916579614562i,-0.54091513103870+0.46000078868751i);Zpg(zero=c(firstHalfZeros,Conj(firstHalfZeros)),pole=c(0),gain=0.00012511398639)",
                                  "Zpg(zero=c(0.9049098+0.1414979i,0.9049098-0.1414979i,1.192327), pole=c(0.9588639+0.7240575i,0.9588639-0.7240575i,1.511628), gain=1)",
                                  "p=3;n=5;sgolay(p,n) %*% c(1,rep(0,times=n-1))",
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
                                  "gausswin(41,5)",
                                  "hanning(41)",
                                  "hamming(41)",
                                  "triang(41)",
                                  "N=18;leftside=sin(pi*(0.3*(-(N/2):(-1))))/(pi*(0.3*(-(N/2):(-1))));c(leftside,1,rev(leftside))*blackman(N+1)", # "N=18;sinc(0.3*(-(N/2):(N/2)))*0.3*blackman(N+1)",
                                  "spencerFilter()",
                                  "Ma(b=c(-3, -6, -5, 3, 21, 46, 67, 74, 67, 46, 21, 3, -5, -6, -3) / 320)",
                                  sep = "','"
                                ),
                                "'), 1 )"
                              )
                            ),
                            selected = sample(
                              c(
                                "omegac=0.4;omegacprime=tan(omegac*pi/2);Zpg(zero=c(-1), pole=c(-(omegacprime-1)/(omegacprime+1)), gain=omegacprime/(omegacprime+1))",
                                "omegac=0.4;signal::bilinear(Sz=c(-fpmaxx/10),Sp=c(-tan(pi*omegac/2)),Sg=tan(pi*omegac/2)/(fpmaxx/10),T=2)",
                                "omegac1=0.3;omegac2=0.7;signal::sftrans(Sz=c(-1e16),Sp=c(-tan(pi*omegac1/2)),Sg=tan(pi*omegac1/2)/(1e16),W=c(omegac1,omegac2),stop=FALSE)",
                                "butter(n=5,W=0.3,type=\"low\")",
                                "butter(n=5,W=0.7,type=\"high\")",
                                "butter(n=5,W=c(0.3,0.7),type=\"pass\")",
                                "butter(n=5,W=c(0.3,0.7),type=\"stop\")",
                                "Arma(b=c(1), a=c(1,0,(1-eps)))",
                                "theta=2/3;Zpg(zero=c(0), pole=c((1-eps)*exp(theta*pi*1i),(1-eps)*exp(-theta*pi*1i)), gain=1)",
                                "fir1(n=40,w=0.3,type=\"low\",window=hamming(41),scale=TRUE)",
                                "fir1(n=10,w=c(0.3,0.7),type=\"pass\",window=hamming(11),scale=TRUE)",
                                "fir1(n=10,w=c(0.3,0.7),type=\"stop\",window=hamming(11),scale=TRUE)",
                                "fir2(n=100, f=c(0, 0.3, 0.3, 0.7, 0.7, 1), m=c(0, 0, 1, 0.5, 0, 0), grid_n=512, ramp_n=5, window=hamming(101))",
                                "FftFilter(rep(1/5,times=5),n=512)$b",
                                "N=5;Arma(b=c(1/N,rep(0,times=N-1),-1/N),a=c(1,-(1-eps)))",
                                "R=5;M=1;Arma(b=c(1,rep(0,times=R*M-1),-1), a=c(1,-(1-eps)))",
                                "N=8;Zpg(zero=c(1,-1,1i,-1i,1/sqrt(2)+1/sqrt(2)*1i,1/sqrt(2)-1/sqrt(2)*1i,-1/sqrt(2)+1/sqrt(2)*1i,-1/sqrt(2)-1/sqrt(2)*1i), pole=c(1-eps), gain=1/N)",
                                "N=12;D=7;fc=0.7;L=2*N;FF=matrix(data=0,nrow=N,ncol=1);for (k in seq(1,N,by=1)){f=2*k/(L+1);if (f <= fc) {FF[k]=exp(-1i*D*f*pi)}};Fb=Conj(FF);FF=c(1,FF,pracma::flipud(Fb));h=Re(pracma::ifft(FF));r1=t(h);r=c(r1[1],pracma::fliplr(as.matrix(t(r1[2:length(r1)]))));H=pracma::Toeplitz(h,r);H0=H[,1:(N+1)];H1=H0[1:(N+1),];h1=H0[seq((N+2),(L+1)),1];H2=H0[seq((N+2),(L+1)),seq(2,(N+1))];ah=-pracma::inv(H2) %*% h1;a=c(1,ah);b=H1 %*% a;list(a=a,b=b)",
                                "Arma(b=c(1,1), a=c(1,-(1-eps)))",
                                "N=12;D=7;L=120;fc=0.7;L1=0.5*L;FF=matrix(data=0,nrow=L1,ncol=1);for (k in seq(1,L1)){f=2*k/(L+1);if (f <= fc) {FF[k]=exp(-1i*D*f*pi)}};Fb=Conj(FF);FF=c(1,FF,pracma::flipud(Fb));h=Re(pracma::ifft(FF));r1=t(h);r=c(r1[1],pracma::fliplr(as.matrix(t(r1[2:length(r1)]))));H=pracma::Toeplitz(h,r);H0=H[,1:(N+1)];H1=H0[1:(N+1),];h1=H0[seq((N+2),(L+1)),1];H2=H0[seq((N+2),(L+1)),seq(2,(N+1))];ah=-pracma::inv(t(H2) %*% H2) %*% t(H2) %*% h1;a=c(1,ah);b=H1 %*% a;list(a=a,b=b)",
                                "Ma(b=c(1/3,2/3,1/3))",
                                "Arma(b=c(1/3,2/3,1/3), a=c(1,1-eps))",
                                "Zpg(zero=c(-1,-1), pole=c(-(1-eps)), gain=1/3)",
                                "Ma(b=c(1,-fpmaxx))",
                                "Arma(b=c(1,0,0, 0.5^3), a=c(1,0,0,0,0, 0.9^5))",
                                "R=8;M=1;Arma(b=c(1,rep(0,times=R*M-1),-1), a=c(1,-(1-eps)))",
                                "r1=1.3;th1=0.6;Zpg(zero=c(r1*exp(th1*pi*1i),r1*exp(-th1*pi*1i)), pole=c((1/r1)*exp(th1*pi*1i),(1/r1)*exp(-th1*pi*1i)), gain=1)",
                                "Arma(b=c(1,-fpmaxx), a=c(1))",
                                "myb=c(1,2,3,4,5,6);Arma(b=myb, a=rev(myb))",
                                "Zpg(zero=c(0.9*exp(0.6*pi*1i),0.9*exp(-0.6*pi*1i),0.8*exp(0.8*pi*1i),0.8*exp(-0.8*pi*1i)), pole=c(0), gain=1)",
                                "Zpg(zero=c(1/0.9*exp(0.6*pi*1i),1/0.9*exp(-0.6*pi*1i),1/0.8*exp(0.8*pi*1i),1/0.8*exp(-0.8*pi*1i)), pole=c(0), gain=1)",
                                "t=seq(0,2-1/512,by=1/512);fs=512;Ts=1/fs;N=23;M=(N-1)/2;n=1:M;h=cos(n*pi)/(Ts*n);h=c(-pracma::fliplr(as.matrix(t(h))),0,h);win=signal::hamming(N);win*h",
                                "N=41;M=20;hz=matrix(data=0,nrow=1,ncol=M);zw=seq(1,(M-1),by=2);hz[seq(1,(M-1),by=2)]=2 / (pi*zw);hd=c(-pracma::fliplr(as.matrix(hz)),0,hz);w=signal::hamming(N);hd*w",
                                "fs=512;Ts=1/fs;N1=23;M=(N1-1)/2;n=seq(0,M-1,by=1);k=M-n;k2=k^2;fc=0.3*pi;h1=sin(k*fc);h2=(fc*k)*cos(k*fc);hd=(h1-h2)/(Ts*pi*k2);hd=c(hd,0,-pracma::fliplr(as.matrix(t(hd))));win=signal::hamming(N1);h_bld=win*hd",
                                "omega0=0.22*pi;D=2*pi/omega0;rho=0.99;Arma(b=c(1,rep(0,times=floor(D-1)),-1), a=c(1,rep(0,times=floor(D-1)),-(rho)^D))",
                                "cheby1(n=5,Rp=3,W=0.7,type=\"high\")",
                                "cheby1(n=5,Rp=3,W=0.3,type=\"low\")",
                                "cheby1(n=5,Rp=3,W=c(0.3,0.7),type=\"pass\")",
                                "cheby1(n=5,Rp=3,W=c(0.3,0.7),type=\"stop\")",
                                "cheby2(n=5,Rp=20,W=0.3,type=\"low\")",
                                "cheby2(n=5,Rp=20,W=0.7,type=\"high\")",
                                "cheby2(n=5,Rp=20,W=c(0.3,0.7),type=\"pass\")",
                                "cheby2(n=5,Rp=20,W=c(0.3,0.7),type=\"stop\")",
                                "ellip(n=5,Rp=3,Rs=40,W=0.3,type=\"low\")",
                                "ellip(n=5,Rp=3,Rs=40,W=0.7,type=\"high\")",
                                "ellip(n=5,Rp=3,Rs=40,W=c(0.3,0.7),type=\"pass\")",
                                "ellip(n=5,Rp=3,Rs=40,W=c(0.3,0.7),type=\"stop\")",
                                "butter(n=buttord(Wp=0.285, Ws=0.345, Rp=0.5, Rs=29))",
                                "cheby1(n=cheb1ord(Wp=0.3, Ws=0.34, Rp=0.5, Rs=29))",
                                "ellip(n=ellipord(Wp=0.285, Ws=0.345, Rp=0.5, Rs=29))",
                                "remez(n=15, f= c(0, 0.3, 0.4, 1), a= c(1,1, 0,0), ftype= \"bandpass\")",
                                "firstHalfCoef=c(0.034025544,0.006219216,-0.005305575,0.006128687,-0.005593423,0.00624262,-0.006848848,0.008979105,-0.008978654,0.017501073,-0.006953636,0.039774499,-0.064655981,0.085240952,-0.131292156);Ma(b=c(firstHalfCoef,0.195140968,rev(firstHalfCoef)))",
                                "firstHalfCoef=c(0.00012511398639,0.00001335284427,0.00016015250121,0.00000634686622,0.00026201837991,0.00007281852105,0.00045629795460,0.00022819555936,0.00071588589103,0.00047316021190,0.00110189764986,0.00088563032407,0.00184819117706,0.00188050116629,0.00282392666400,0.00363976768981,0.00591155524557,0.00644303257612,0.01406190034797,0.00537162176461,0.03594691432517,0.06164502638211,0.08276620944465,0.13009560635626);Ma(b=c(firstHalfCoef,0.19452719610477,rev(firstHalfCoef)))",
                                "firstHalfZeros=c(-1.27851808211318+0.62684824819101i,-1.36614299238589+0.27717114439577i,-1.07284006705681+0.91235620647926i,-0.77267104757162+1.12541933041583i,-0.44961436355736+1.22363585379352i,-0.20330452525073+1.29600355151311i,0.14750702212683+1.34841369533889i,0.50634734941036+1.25979827310114i,0.83329614995319+1.04982507756824i,1.04560310563666+0.67293837999034i,1.20771535897979+0.24059979456907i,0.95139216401989+0.56315210274529i,0.77837030003487+0.46073626392613i,0.79640192035334+0.15865836018951i,0.67627030631208+0.43523995090672i,0.46384030886663+0.58436750039291i,0.27466939387352+0.68338074343342i,0.08016782642482+0.73284236586261i,-0.11813451163119+0.75307102211071i,-0.26456653442537+0.72002392155667i,-0.41461446573694+0.60389882069075i,-0.70304843505842+0.14263861132902i,-0.63057376627246+0.30916579614562i,-0.54091513103870+0.46000078868751i);Zpg(zero=c(firstHalfZeros,Conj(firstHalfZeros)),pole=c(0),gain=0.00012511398639)",
                                "Zpg(zero=c(0.9049098+0.1414979i,0.9049098-0.1414979i,1.192327), pole=c(0.9588639+0.7240575i,0.9588639-0.7240575i,1.511628), gain=1)",
                                "p=3;n=5;sgolay(p,n) %*% c(1,rep(0,times=n-1))",
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
                                "gausswin(41,5)",
                                "hanning(41)",
                                "hamming(41)",
                                "triang(41)",
                                "N=18;leftside=sin(pi*(0.3*(-(N/2):(-1))))/(pi*(0.3*(-(N/2):(-1))));c(leftside,1,rev(leftside))*blackman(N+1)", # "N=18;sinc(0.3*(-(N/2):(N/2)))*0.3*blackman(N+1)",
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
                              condition = "input.coordsheaderExport",
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
                          width = 6,
                          uiOutput(outputId = "slider2AWidget"),
                          uiOutput(outputId = "stretchyslider2ArangeWidget"),
                          uiOutput(outputId = "stretchyslider2AstepWidget"),
                          uiOutput(outputId = "slider2AanimintervalWidget")
                        ),
                        column(
                          width = 6,
                          uiOutput(outputId = "slider2BWidget"),
                          uiOutput(outputId = "stretchyslider2BrangeWidget"),
                          uiOutput(outputId = "stretchyslider2BstepWidget"),
                          uiOutput(outputId = "slider2BanimintervalWidget")
                        )
                      )),
                      fluidRow(
                        column(
                          width = 6,
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
                                size = 10
                              )
                            )
                          )
                        ),
                        column(
                          width = 6,
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
                                size = 10
                              )
                            )
                          )
                        )
                      )
                    ),
                    bsAlert(anchorId = "AlertMsgToUser")
                  )
                )
              ),
              # .. mainPanel ----
              mainPanel(
                id = "mainPanel",
                style = paste0("background-color: ", MainSidePanelBackgroundColor,
                               ";"),
                width = 12 - mySidebarWidth,
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
                            height = "700px",
                            inline = FALSE,
                            hover = hoverOpts(
                              id = "magplot_hover",
                              delay = 200,
                              delayType = "throttle"
                            )
                          ),
                          uiOutput("hover_info2"),
                          plotOutput(
                            outputId = "axes_magpassbandstopband",
                            width = "100%",
                            height = "700px",
                            inline = FALSE
                          ),
                          tags$div(
                            title = "tooltip: The chosen-range, [min, max], can be dragged together, as one piece",
                            column(
                              width = 6,
                              align = "center",
                              uiOutput(outputId = "zoomlimXpassbandWidget"),
                              uiOutput(outputId = "zoomlimYpassbandWidget")
                            ),
                            column(
                              width = 6,
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
                                height = "700px",
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
                            height = "600px",
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
                            height = "600px",
                            inline = FALSE
                          )
                        )
                      ),
                      tabPanel(
                        style = paste0("background-color: ",
                                       TabPanelBackgroundColor, ";"),
                        title = "Imp h[n]",
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
                            height = "600px",
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
                              value = 25,
                              min = 2,
                              max = 1001,
                              step = 1
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
                            title = "tooltip: Summary All-in-1 Tab of Plots Page",
                            helpText("Welcome to the Summary All-in-1 Tab..."),
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
                              cols = 80,
                              rows = 8,
                              placeholder = "Enter any text that you want to appear at the start of the report\n(Note: RMarkdown/ LaTeX formatting markup is acceptable)\nCan drag the corner to expand entry-area's size",
                              resize = NULL
                            ),
                            sliderInput(inputId = "slider", 
                                        label="Example Control-Input (e.g. a Slider)",
                                        min=1, 
                                        max=100, 
                                        value=50
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
            )
          )),
          withMathJax(),
          shinyBS::bsCollapse(
            id = "FnBoxesCollapse",
            multiple = TRUE,
            shinyBS::bsCollapsePanel(
              value = "TrFnCollapse",
              title = "Transfer-Function, (z)-Domain (click to expand/ collapse):",
              style = "info",
              fluidRow(
                tags$span(
                  title = "tooltip: z-transform is most useful when the infinite-sum can be expressed as a simple mathematical formula. One important form of representation is to represent it as a rational-function (as shown) where the numerator and denominator are polynomials in z. The values of z for which H(z)=0 are called the zeros of H(z), and the values of z for which H(z) is infinity are referred to as the poles of H(z). The zeros are the roots of the numerator-polynomial, and the poles of H(z) (for finite-values of z) are the roots of the denominator-polynomial.  A plot of Poles and Zeros of a system on the z-plane is called a Pole-Zero plot.\n(use MathJax's right-mouse-click/ context-menu for download into your (La)TeX/ MathML (e.g. MS Word) documents and/or for zooming-options)",
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
                          value = 1,
                          min = 0,
                          max = 10,
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
                    tags$div(
                      title = "tooltip: IIR digital-filter, Direct-Form I\n(use browser's right-mouse-click/ context-menu for image download-options)",
                      img(src = "DirectFormI.png", align = "right", width = "100%"
                        , alt = "IIR digital-filter, Direct-Form I"
                      )
                    ),
                    hr()
                  )
                )
              ),
              hr(),
              tags$span(title = "tooltip: b/a filter-coefficient editing",
                        fluidRow(
                          column(
                            width = 6,
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
                                  label = "b Coefficients (moving-average MA)",
                                  choices = c("0"),
                                  selectize = FALSE,
                                  size = 10
                                )
                              )
                            )
                          ),
                          column(
                            width = 6,
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
                                  label = "a Coefficients (autoregressive AR)",
                                  choices = c("0"),
                                  selectize = FALSE,
                                  size = 10
                                )
                              )
                            )
                          )
                        ))
            )
          )
        ),
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
            helpText("Welcome to the Background/ Theory Page...")
          ),
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
              colourpicker::colourInput(
                inputId = "grcolor",
                label = "Grid-Colour for plots (or just choose 'transparent' for none)",
                palette = "limited",
                allowedCols = grey.colors(40, start = 0, end = 1),
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
                value = 3,
                min = 0.5,
                max = 6,
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
                  `0 --> 1` = "zero2one",
                  `0 --> 3.1415... (\\(=\\pi\\))` = "zero2pi",
                  `0 --> 6.2832... (\\(=2\\pi\\)), one-sided` = "zero22pi",
                  `0 --> 0.5` = "zero2half",
                  `0 --> fs/2 (specify)` = "zero2fsby2",
                  `0 --> \\(f_{max}\\), one-sided (specify)` = "zero2fmax"
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
                    2,
                  min = 1000,
                  max = 3e+12,
                  step = 100
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
        ),
        # 'About' tabPanel ----
        tabPanel(
          style = paste("background-color:", NavBarPageBackgroundColor, ";"),
          title = "About",
          icon = shiny::icon("info", lib = "font-awesome"),
          tags$span(
            title = "tooltip: About Page/ Tab",
            helpText("Welcome to the ",
                     tags$em("About"), " Page..."),
            br(),
            h1("Introducing Shiny"),
            p(
              "Shiny is a new package from RStudio that makes it ",
              em("incredibly easy"),
              " to build interactive web-applications with R."
            ),
            a(img(
              src = "bigorb.png",
              height = 72,
              width = 72
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
            br(),
            tags$div(HTML(
              paste(
                tags$span(style = "color:red",
                          "This text"),
                " is ",
                tags$span(style = "color:red",
                          "red"),
                sep = ""
              )
            ))
          )
        ),
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
  handles <-
    reactiveValues(
      poleloc = c(0),
      zeroloc = c(0),
      selectedpole = NULL,
      selectedzero = NULL,
      maxZoomlimXpassband = 1 * isolate(input$samplingfreq) / 2,
      maxZoomlimYpassband = 1,
      maxZoomlimXstopband = 1 * isolate(input$samplingfreq) / 2,
      maxZoomlimYstopband = 1,
      minZoomlimXpassband = 0 * isolate(input$samplingfreq) / 2,
      minZoomlimYpassband = -1,
      minZoomlimXstopband = 0 * isolate(input$samplingfreq) / 2,
      minZoomlimYstopband = -1,
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
      minslider1range = 0 *
        isolate(input$samplingfreq) / 2,
      maxslider1range = 5 * isolate(input$samplingfreq) / 2,
      minslider1step = 0.001,
      maxslider1step = 0.5,
      minslider1animinterval = 0,
      maxslider1animinterval = 2000,
      minslider2Arange = 0,
      maxslider2Arange = 5,
      minslider2Astep = 0.001,
      maxslider2Astep = 1,
      minslider2Aaniminterval = 0,
      maxslider2Aaniminterval = 2000,
      slider2Avalue = 0.5,
      minslider2Brange = 0,
      maxslider2Brange = 5,
      minslider2Bstep = 0.001,
      maxslider2Bstep = 1,
      minslider2Baniminterval = 0,
      maxslider2Baniminterval = 2000,
      slider2Bvalue = 0.5
    )
  
  # observeEvent resetAll ----
  observeEvent(eventExpr = input$resetAll, handlerExpr = {
    shinyjs::reset("myMainFluidPage")
    shinyjs::info("Done.  All input-elements\n are now reset.")
  })
  
  # observeEvent (button) pb_mp ----
  observeEvent(eventExpr = input$pb_mp, handlerExpr = {
    updateSelectInput(session, inputId = "listbox_pole", choices = c(0))
    handles$selectedpole <- NULL
    handles$selectedzero <- NULL
    handles$poleloc <- c(0)
    if (!pracma::isempty(handles$connecttype)) {
      polestring <- which(handles$connecttype == "x")
      handles$connection[polestring,] <- 0
      handles$connection[, polestring] <- 0
      handles$connecttype[polestring] <- NA
    }
    updateTabsetPanel(session, inputId = "tabPoleZeroEditing", selected = "RealImag")
  })
  
  # observeEvent (button) pb_mz ----
  observeEvent(eventExpr = input$pb_mz, handlerExpr = {
    updateSelectInput(session, inputId = "listbox_zero", choices = c(0))
    handles$selectedzero <- list(XData = NULL, YData = NULL)
    handles$zeroloc <- c(0)
    if (!pracma::isempty(handles$connecttype)) {
      zerostring <- which(handles$connecttype == "o")
      handles$connection[zerostring,] <- 0
      handles$connection[, zerostring] <- 0
      handles$connecttype[zerostring] <- NA
    }
    updateTabsetPanel(session, inputId = "tabPoleZeroEditing", selected = "RealImag")
  })
  
  # observeEvent (button) pb_ma ----
  observeEvent(eventExpr = input$pb_ma, handlerExpr = {
    updateSelectInput(session, inputId = "listbox_pole", choices = c(0))
    updateSelectInput(session, inputId = "listbox_zero", choices = c(0))
    updateNumericInput(session, inputId = "edit_gain", value = 1)
    handles$selectedpole <- list(XData = NULL, YData = NULL)
    handles$selectedzero <- list(XData = NULL, YData = NULL)
    handles$poleloc <- c(0)
    handles$zeroloc <- c(0)
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
                   filtb[Im(filtb) > 1e+12] <- Re(filtb) + j * 1e+12
                   filtb[Im(filtb) < -1e+12] <- Re(filtb) - j * 1e+12
                   filta[Im(filta) > 1e+12] <- Re(filtb) + j * 1e+12
                   filta[Im(filta) < -1e+12] <- Re(filtb) - j * 1e+12
                   rv <- signal::freqz(
                     filtb,
                     filta,
                     region = "whole",
                     n = 2 ^ 20,
                     Fs = 2 * pi * input$samplingfreq / 2
                   )
                   updateNumericInput(session, inputId = "edit_gain", value = 1 / max(Re(rv$h)))
                 }
                 else {
                   updateNumericInput(session, inputId = "edit_gain", value = 1)
                 }
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
                   chosenFilter <- eval(parse(text = input$commonFilters))
                   updateSelectInput(session, inputId = "commonFilters", selected = chosenFilter)
                   return(NULL)
                 }
                 else {
                   chosenFilter <- input$commonFilters
                 }
                 if (grepl("Zpg|sftrans|bilinear", chosenFilter)) {
                   zpg <- eval(parse(text = chosenFilter))
                   handles$zeroloc <- zpg$zero
                   handles$poleloc <- zpg$pole
                   updateNumericInput(session, inputId = "edit_gain", value = zpg$gain)
                   b <- zpg$gain
                   length(b) <- 1
                 }
                 else if (grepl(
                   "fir1|fir2|remez|spencerFilter|sgolay|Ma|chebwin|kaiser|bartlett|blackman|boxcar|flattopwin|gausswin|hanning|hamming|triang|sinc",
                   chosenFilter
                 )) {
                   handles$poleloc <- c(0)
                   b <- eval(parse(text = chosenFilter))
                   if (length(b) < 2) {
                     handles$zeroloc <- c(0)
                   }
                   else {
                     handles$zeroloc <- polyroot(rev(b)) # numerical stability may be an issue for all but low-degree polynomials
                   }
                 }
                 else if (grepl("FftFilter", chosenFilter)) {
                   handles$poleloc <- c(0)
                   b <- eval(parse(text = chosenFilter))
                   if (length(b) < 2) {
                     handles$zeroloc <- c(0)
                   }
                   else {
                     handles$zeroloc <- polyroot(rev(b)) # numerical stability may be an issue for all but low-degree polynomials
                   }
                 }
                 else {
                   a <- eval(parse(text = paste0(chosenFilter, "$a")))
                   if (length(a) < 2) {
                     handles$poleloc <- c(0)
                   }
                   else {
                     handles$poleloc <- polyroot(rev(a)) # numerical stability may be an issue for all but low-degree polynomials
                   }
                   b <- eval(parse(text = paste0(chosenFilter, "$b")))
                   if (length(b) < 2) {
                     handles$zeroloc <- c(0)
                   }
                   else {
                     handles$zeroloc <- polyroot(rev(b)) # numerical stability may be an issue for all but low-degree polynomials
                   }
                 }
                 updateNumericInput(session, inputId = "edit_gain", value = if (abs(b[1]) >=
                                                                                eps) {
                   abs(b[1])
                 }
                 else {
                   filtb <- handlesb()
                   filta <- handlesa()
                   filtb[Re(filtb) > 1e+12] <- 1e+12
                   filtb[Re(filtb) < -1e+12] <- -1e+12
                   filta[Re(filta) > 1e+12] <- 1e+12
                   filta[Re(filta) < -1e+12] <- -1e+12
                   filtb[Im(filtb) > 1e+12] <- Re(filtb) + j * 1e+12
                   filtb[Im(filtb) < -1e+12] <- Re(filtb) - j * 1e+12
                   filta[Im(filta) > 1e+12] <- Re(filtb) + j * 1e+12
                   filta[Im(filta) < -1e+12] <- Re(filtb) - j * 1e+12
                   rv <- signal::freqz(
                     filtb,
                     filta,
                     region = "whole",
                     n = 2 ^ 20,
                     Fs = 2 * pi * input$samplingfreq / 2
                   )
                   1 / max(Re(rv$h))
                 })
                 updateCheckboxInput(session, inputId = "normalizedMagPlotAmplitude",
                                     value = TRUE)
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
                 if ((pL + zL) > 0)
                   handles$connection <- diag(pL + zL)
                 handles$connecttype <- NULL
                 if (pL > 0)
                   for (k in (1:pL)) {
                     handles$connecttype <- c(handles$connecttype, "x")
                   }
                 if (zL > 0)
                   for (k in ((pL + 1):(pL + zL))) {
                     handles$connecttype <- c(handles$connecttype, "o")
                   }
                 if (!pracma::isempty(Mypoles)) {
                   for (ii in (1:pL)) {
                     conjp <- which((round(handlestol * Mypoles) / handlestol) ==
                                      (Conj((
                                        round(handlestol * Mypoles[ii]) / handlestol
                                      ))))
                     if (!pracma::isempty(conjp)) {
                       handles$connection[ii, conjp] <- 1
                       polezero1 <-
                         which(
                           round(handlestol * Myzeros) / handlestol ==
                             round(handlestol * Re((
                               1 / c(Mypoles[ii])
                             ))) / handlestol +
                             round(handlestol * Im((
                               1 / c(Mypoles[ii])
                             )) * (0 + 1i)) / handlestol
                         )
                       polezero2 <-
                         which(
                           round(handlestol * Myzeros) / handlestol ==
                             round(handlestol * Re((
                               1 / c(Mypoles[ii])
                             ))) / handlestol -
                             round(handlestol * Im((
                               1 / c(Mypoles[ii])
                             )) * (0 + 1i)) / handlestol
                         )
                       if (!pracma::isempty(polezero1)) {
                         handles$connection[ii, pL + polezero1] <- 1
                         handles$connection[ii, pL + polezero2] <- 1
                       }
                     }
                   }
                 }
                 if (!pracma::isempty(Myzeros)) {
                   for (ii in (pL + 1):(pL + zL)) {
                     conjz <- which(round(handlestol * Myzeros) / handlestol ==
                                      Conj((
                                        round(handlestol * Myzeros[ii - pL]) / handlestol
                                      )))
                     if (!pracma::isempty(conjz)) {
                       handles$connection[ii, conjz + pL] <- 1
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
      value = c(0, 0.5) * input$samplingfreq / 2,
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
      animate = animationOptions(interval = inputslider1animinterval,
                                 loop = FALSE),
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
      value = c(0, 0.5),
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
      step = 50,
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
      value = c(0, 0.5),
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
      step = 50,
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
                           3 * inputstretchyslider1step,
                         round((
                           handles$maxslider1range -
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
                           3 * inputstretchyslider1step,
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
                   matchpoint <- match(tgt, handles$zeroloc, nomatch = -1)
                   if ((matchpoint > 0) &&
                       (matchpoint <= length(handles$zeroloc))) {
                     if (input$tabPoleZeroEditing == "rtheta") {
                       if ((is.null(input$edit_currentSelectionText)) ||
                           (is.na(input$edit_currentSelectionText))) {
                         valueToReplace <- input$slider2A * exp(j * input$slider2B)
                       }
                       else {
                         valueToReplace <-
                           eval(parse(text = input$edit_currentSelectionText))
                       }
                     }
                     else {
                       if ((is.null(input$edit_currentSelectionText)) ||
                           (is.na(input$edit_currentSelectionText))) {
                         n1 <- input$slider2A
                         n2 <- input$slider2B
                         if (is.infinite(Im(n2))) {
                           valueToReplace <- complex(1, n1, switch(sign(n2) +
                                                                     2,-Inf, 0, Inf))
                         }
                         else if (is.infinite(Re(n2))) {
                           valueToReplace <- complex(1, n1, switch(sign(n2) +
                                                                     2,-Inf, 0, Inf))
                         }
                         else {
                           valueToReplace <- n1 + j * n2
                         }
                       }
                       else {
                         valueToReplace <-
                           eval(parse(text = input$edit_currentSelectionText))
                       }
                     }
                   }
                   if ((Mod(valueToReplace) > 1e-06)) {
                     if (abs(Im(valueToReplace)) == 0) {
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
                       ui = "Value is too small...",
                       duration = 3,
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
                           3 * input$stretchyslider2Astep,
                         round((
                           handles$maxslider2Arange -
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
                           3 * input$stretchyslider2Astep,
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
                   if (10 * handles$minslider2Astep < handles$maxslider2Astep / (1 +
                                                                                 handles$growSliders)) {
                     handles$minslider2Astep <- 10 * handles$minslider2Astep
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
                   handles$minslider2Astep <- max(c(1e-06, handles$minslider2Astep / 10))
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Astep",
                     value = input$stretchyslider2Astep,
                     min = handles$minslider2Astep
                   )
                   handles$maxslider2Astep <- max(c(
                     input$stretchyslider2Astep +
                       3 * 1e-06,
                     round((
                       handles$maxslider2Astep - (handles$maxslider2Astep -
                                                    handles$minslider2Astep) * handles$growSliders
                     ) / 1e-06
                     ) *
                       1e-06
                   ))
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
                           3 * input$stretchyslider2Bstep,
                         round((
                           handles$maxslider2Brange -
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
                           3 * input$stretchyslider2Bstep,
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
                   if (10 * handles$minslider2Bstep < handles$maxslider2Bstep / (1 +
                                                                                 handles$growSliders)) {
                     handles$minslider2Bstep <- 10 * handles$minslider2Bstep
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
                   handles$minslider2Bstep <- max(c(1e-06, handles$minslider2Bstep / 10))
                   updateSliderInput(
                     session,
                     inputId = "stretchyslider2Bstep",
                     value = input$stretchyslider2Bstep,
                     min = handles$minslider2Bstep
                   )
                   handles$maxslider2Bstep <- max(c(
                     input$stretchyslider2Bstep +
                       3 * 1e-06,
                     round((
                       handles$maxslider2Bstep - (handles$maxslider2Bstep -
                                                    handles$minslider2Bstep) * handles$growSliders
                     ) / 1e-06
                     ) *
                       1e-06
                   ))
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
      value = c(0, 0.5) * input$samplingfreq / 2,
      step = handles$stepZoomlimXpassband,
      round = -2,
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
      value = c(-3, 0),
      step = handles$stepZoomlimYpassband,
      round = -2,
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
      value = c(0.5, 1) * input$samplingfreq / 2,
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
      value = c(-130,-60),
      step = handles$stepZoomlimYstopband,
      round = 1,
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
                     )
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
                     )
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
                     )
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
                     )
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
                   updateNumericInput(session, inputId = "samplingfreq", value = 2)
                   handles$minZoomlimXpassband <- 0
                   handles$maxZoomlimXpassband <- 1
                   handles$minZoomlimXstopband <- 0
                   handles$maxZoomlimXstopband <- 1
                   handles$minslider1range <- 0
                   handles$maxslider1range <- 1
                   slider1value <- 0.5
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXpassband",
                     value = c(0,
                               0.5),
                     min = 0,
                     max = handles$maxZoomlimXpassband,
                     step = 0.05
                   )
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXstopband",
                     value = c(0.5,
                               1),
                     min = 0,
                     max = 1,
                     step = 0.05
                   )
                 }
                 else if (input$freqaxisunits == "zero2pi") {
                   updateNumericInput(session, inputId = "samplingfreq", value = 2 *
                                        pi)
                   handles$maxZoomlimXpassband <- pi
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXpassband",
                     value = c(0,
                               pi /
                                 2),
                     min = 0,
                     max = handles$maxZoomlimXpassband,
                     step = pi / 12
                   )
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXstopband",
                     value = c(0,
                               pi /
                                 2),
                     min = 0,
                     max = pi,
                     step = pi / 12
                   )
                 }
                 else if (input$freqaxisunits == "zero22pi") {
                   updateNumericInput(session, inputId = "samplingfreq", value = 4 *
                                        pi)
                   updateCheckboxInput(session, inputId = "twosidedFFT", value = FALSE)
                   updateCheckboxInput(session, inputId = "FFTshifted", value = FALSE)
                 }
                 else if (input$freqaxisunits == "zero2half") {
                   updateNumericInput(session,
                                      inputId = "samplingfreq",
                                      value = 1 *
                                        input$samplingfreq / 2)
                 }
                 else if (input$freqaxisunits == "zero2piby2") {
                   updateNumericInput(session, inputId = "samplingfreq", value = pi)
                 }
                 else if ((input$freqaxisunits == "zero2fs") ||
                          (input$freqaxisunits ==
                           "zero2fsby2")) {
                   updateNumericInput(session, inputId = "samplingfreq", value = 44100)
                   handles$minZoomlimXpassband <- 0
                   handles$maxZoomlimXpassband <- 44100 / 2
                   handles$minZoomlimXstopband <- 0
                   handles$maxZoomlimXstopband <- 44100 / 2
                   handles$minslider1range <- 0
                   handles$maxslider1range <- 44100
                   slider1value <- (44100 / 2) / 2
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXpassband",
                     value = c(0,
                               0.5) * input$samplingfreq /
                       2,
                     min = 0,
                     max = handles$maxZoomlimXpassband,
                     step = 0.05
                   )
                   updateSliderInput(
                     session,
                     inputId = "zoomlimXstopband",
                     value = c(0.5,
                               1) * input$samplingfreq /
                       2,
                     min = 0,
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
      rawList <- readLines(con = inpFile$datapath)
      Npoles <- match("#zerolocs", rawList) - match("#polelocs",
                                                    rawList) - 1
      importedPoles <- scan(
        file = inpFile$datapath,
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
          file = inpFile$datapath,
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
        max(length(importedPoles), length(importedZeros))
       # http://stackoverflow.com/questions/19074163/cbind-is-there-a-way-to-have-missing-values-set-to-na
      length(importedPoles) <- maxlengthimported
      length(importedZeros) <- maxlengthimported
      print(cbind(Zeros = importedZeros, Poles = importedPoles),
            na.print = "")
    }
    else if (input$textbinaryformatImport == "bin") {
      zz <- file(inpFile$datapath, open = "rb")
      close(zz)
    }
    else if (input$textbinaryformatImport == "RData") {
      load(file = inpFile$datapath, verbose = TRUE)
    }
    else if (input$textbinaryformatImport == "mat") {
      data <- R.matlab::readMat(inpFile$datapath)
    }
    else if (input$textbinaryformatImport == "yml") {
      data <- yaml::yaml.load_file(inpFile$datapath)
      unlink(inpFile$datapath)
    }
  })
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
                                            1:length(
                                              handles$poleloc
                                            )), paste0("z", 1:length(
                                              handles$zeroloc
                                            ))))
                 point <-
                   nearPoints(
                     df = Mydf,
                     coordinfo = input$pzplot_dblclick,
                     xvar = "xReal",
                     yvar = "yImag",
                     threshold = 7,
                     maxpoints = 1,
                     addDist = TRUE
                   )
                 if (!(handles$inDragMode) && (NROW(point) == 0))
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
                     ui = paste0("Point [", point[1, 1], ", ",
                                 point[1, 2], "j] is now selected..."),
                     duration = 3,
                     closeButton = TRUE,
                     type = "warning"
                   )
                   handles$inDragMode <- TRUE
                   currentpt <- c(input$pzplot_hover$x, input$pzplot_hover$y)
                   x <- currentpt[1]
                   y <- currentpt[2]
                   if ("real_motionT" %in% input$DragLockGrp) {
                     y <- 0
                   }
                   if ("circular_motionT" %in% input$LockCoordsPolarGrp) {
                     handles$circular_radius <- sqrt(x * x + y * y)
                     currenttheta <- atan2(y, x)
                     x <- handles$circular_radius * cos(currenttheta)
                     y <- handles$circular_radius * sin(currenttheta)
                     handleslinerc$xdata <- handles$circular_radius * sin(seq(0,
                                                                              2 * pi, by = 2 * pi /
                                                                                20))
                     handleslinerc$ydata <- handles$circular_radius * cos(seq(0,
                                                                              2 * pi, by = 2 * pi /
                                                                                20))
                   }
                   if ("radial_motionT" %in% input$LockCoordsPolarGrp) {
                     handles$radial_theta <- atan2(y, x)
                     r <- sqrt(x * x + y * y)
                     z <- c(cos(handles$radial_theta),
                            sin(handles$radial_theta))
                     handleslinerr$xdata <- c(0, 2 * z[1])
                     handleslinerr$ydata <- c(0, 2 * z[2])
                     x <- r * z[1]
                     y <- r * z[2]
                   }
                   Mydf <- as.data.frame(cbind(xReal = c(
                     Re(handles$poleloc),
                     Re(handles$zeroloc)
                   ), yImag = c(
                     Im(handles$poleloc), Im(handles$zeroloc)
                   )),
                   row.names = c(paste0("p", 1:length(
                     handles$poleloc
                   )), paste0("z",
                              1:length(
                                handles$zeroloc
                              ))))
                   closestRowsToClickpoint <-
                     nearPoints(
                       df = Mydf,
                       coordinfo = input$pzplot_hover,
                       xvar = "xReal",
                       yvar = "yImag",
                       threshold = 7
                     )
                   if (!pracma::isempty(closestRowsToClickpoint)) {
                     if ((NROW(closestRowsToClickpoint) > 0) &&
                         (startsWith(row.names(closestRowsToClickpoint)[1],
                                     "z"))) {
                       indexOfClickedPoleOrZeroWithinEntireSet <-
                         pracma::str2num(substring(
                           row.names(closestRowsToClickpoint[1,]),
                           first = 2,
                           last = nchar(closestRowsToClickpoint[1,])
                         )) + length(handles$poleloc)
                       handles$subscriptOfClickedPoleOrZero <-
                         indexOfClickedPoleOrZeroWithinEntireSet -
                         length(handles$poleloc)
                       polestring <- which(handles$connecttype == "x")
                       zerostring <- which(handles$connecttype == "o")
                       theseAreConnected <-
                         which(handles$connection[zerostring[handles$subscriptOfClickedPoleOrZero],] != 0)
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
                         for (i in (1:(length(temppoleNumber)))) {
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
                     y <- 0
                   }
                   if ("circular_motionT" %in% input$LockCoordsPolarGrp) {
                     handles$circular_radius <- sqrt(x * x + y * y)
                     currenttheta <- atan2(y, x)
                     x <- handles$circular_radius * cos(currenttheta)
                     y <- handles$circular_radius * sin(currenttheta)
                     handleslinerc$xdata <- handles$circular_radius * sin(seq(0,
                                                                              2 * pi, by = 2 * pi /
                                                                                20))
                     handleslinerc$ydata <- handles$circular_radius * cos(seq(0,
                                                                              2 * pi, by = 2 * pi /
                                                                                20))
                   }
                   if ("radial_motionT" %in% input$LockCoordsPolarGrp) {
                     handles$radial_theta <- atan2(y, x)
                     r <- sqrt(x * x + y * y)
                     z <- c(cos(handles$radial_theta),
                            sin(handles$radial_theta))
                     handleslinerr$xdata <- c(0, 2 * z[1])
                     handleslinerr$ydata <- c(0, 2 * z[2])
                     x <- r * z[1]
                     y <- r * z[2]
                   }
                   Mydf <- as.data.frame(cbind(xReal = c(
                     Re(handles$poleloc),
                     Re(handles$zeroloc)
                   ), yImag = c(
                     Im(handles$poleloc), Im(handles$zeroloc)
                   )),
                   row.names = c(paste0("p", 1:length(
                     handles$poleloc
                   )), paste0("z",
                              1:length(
                                handles$zeroloc
                              ))))
                   closestRowsToClickpoint <-
                     nearPoints(
                       df = Mydf,
                       coordinfo = input$pzplot_hover,
                       xvar = "xReal",
                       yvar = "yImag",
                       threshold = 7
                     )
                   str(closestRowsToClickpoint)
                   if (!(pracma::isempty(closestRowsToClickpoint)) &&
                       (!anyNA(closestRowsToClickpoint)) &&
                       (!("NA" %in% row.names(closestRowsToClickpoint)[1]))) {
                     if ((NROW(closestRowsToClickpoint) > 0) &&
                         (startsWith(row.names(closestRowsToClickpoint)[1],
                                     "z"))) {
                       indexOfClickedPoleOrZeroWithinEntireSet <-
                         pracma::str2num(substring(
                           row.names(closestRowsToClickpoint[1,]),
                           first = 2,
                           last = nchar(closestRowsToClickpoint[1,])
                         )) + length(handles$poleloc)
                       handles$subscriptOfClickedPoleOrZero <-
                         indexOfClickedPoleOrZeroWithinEntireSet -
                         length(handles$poleloc)
                       polestring <- which(handles$connecttype == "x")
                       zerostring <- which(handles$connecttype == "o")
                       theseAreConnected <-
                         which(handles$connection[zerostring[handles$subscriptOfClickedPoleOrZero],] != 0)
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
                         for (i in (1:(length(temppoleNumber)))) {
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
                           c(tempstring[1:(subscriptOfClickedPoleOrZero -
                                             1)], x + y * (0 + 1i), tempstring[seq(
                                               from = (subscriptOfClickedPoleOrZero +
                                                         1),
                                               to = length(tempstring)
                                             )])
                         handles$selectedzero <- list(XData = x, YData = y)
                         handles$selectedpole <- list(XData = NULL, YData = NULL)
                         if (!pracma::isempty(outputParametersFromStartdragToMovepez_zeroconnect)) {
                           z1 <- outputParametersFromStartdragToMovepez_zeroconnect[1]
                           handles$zeroloc[z1] <- x - y * (0 + 1i)
                           tempstring <-
                             c(tempstring[1:(z1 - 1)], x - y * (0 + 1i),
                               tempstring[seq(from = (z1 + 1),
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
                         if (Mod(Im((1 / c(
                           handles$zeroloc[handles$subscriptOfClickedPoleOrZero]
                         ))) -
                         Im(handles$poleloc[outputParametersFromStartdragToMovepez_poleconnect[1]])) <
                         handlestol) {
                           outputParametersFromStartdragToMovepez_type <- 1
                         }
                         else {
                           outputParametersFromStartdragToMovepez_type <- 0
                         }
                         if ((x ^ 2 + y ^ 2) != 1) {
                           x <- x + 1e-06
                         }
                         tempx <- Re(1 / c(x + y * (0 + 1i)))
                         tempy <- Im(1 / c(x + y * (0 + 1i)))
                         subscriptOfClickedPoleOrZero <-
                           handles$subscriptOfClickedPoleOrZero
                         handles$zeroloc[subscriptOfClickedPoleOrZero] <- x +
                           y * (0 + 1i)
                         tempstring <- handles$zeroloc
                         tempstring <-
                           c(tempstring[1:(subscriptOfClickedPoleOrZero -
                                             1)], x + y * (0 + 1i), tempstring[seq(
                                               from = (subscriptOfClickedPoleOrZero +
                                                         1),
                                               to = length(tempstring)
                                             )])
                         handles$selectedzero <- list(XData = x, YData = y)
                         if (!pracma::isempty(outputParametersFromStartdragToMovepez_zeroconnect)) {
                           z1 <- outputParametersFromStartdragToMovepez_zeroconnect[1]
                           handles$zeroloc[z1] <- x - y * (0 + 1i)
                           tempstring <-
                             c(tempstring[1:(z1 - 1)], x + y * (0 + 1i),
                               tempstring[seq(from = (z1 + 1),
                                              to = length(tempstring))])
                           handles$selectedzero <-
                             list(XData = c(x, x), YData = c(y,-y))
                         }
                         updateSelectInput(session, inputId = "listbox_zero",
                                           choices = tempstring)
                         tempstring <- handles$poleloc
                         if (outputParametersFromStartdragToMovepez_type ==
                             1) {
                           p1 <- outputParametersFromStartdragToMovepez_poleconnect[1]
                           handles$poleloc[p1] <- tempx + tempy * (0 + 1i)
                           tempstring <-
                             c(tempstring[1:(p1 - 1)], tempx + tempy *
                                 (0 + 1i), tempstring[seq(from = (p1 + 1),
                                                          to = length(tempstring))])
                           handles$selectedpole <-
                             list(XData = tempx, YData = tempy)
                           if (length(outputParametersFromStartdragToMovepez_poleconnect) >
                               1) {
                             p2 <- outputParametersFromStartdragToMovepez_poleconnect[2]
                             handles$poleloc[p2] <- tempx - tempy * (0 + 1i)
                             tempstring <- c(tempstring[1:(p2 - 1)], tempx -
                                               tempy * (0 + 1i), tempstring[seq(from = (p2 + 1),
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
                             c(tempstring[1:(p1 - 1)], tempx - tempy *
                                 (0 + 1i), tempstring[seq(from = (p1 + 1),
                                                          to = length(tempstring))])
                           handles$selectedpole <-
                             list(XData = tempx, YData = -tempy)
                           if (length(outputParametersFromStartdragToMovepez_poleconnect) >
                               1) {
                             p2 <- outputParametersFromStartdragToMovepez_poleconnect[2]
                             handles$poleloc[p2] <- tempx + tempy * (0 + 1i)
                             tempstring <- c(tempstring[1:(p2 - 1)], tempx +
                                               tempy * (0 + 1i), tempstring[seq(from = (p2 + 1),
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
                     else if ((NROW(closestRowsToClickpoint) > 0) &&
                              (startsWith(row.names(closestRowsToClickpoint)[1],
                                          "p"))) {
                       indexOfClickedPoleOrZeroWithinEntireSet <-
                         pracma::str2num(substring(
                           row.names(closestRowsToClickpoint[1,]),
                           first = 2,
                           last = nchar(closestRowsToClickpoint[1,])
                         ))
                       handles$subscriptOfClickedPoleOrZero <-
                         indexOfClickedPoleOrZeroWithinEntireSet
                       zerostring <- which(handles$connecttype == "o")
                       polestring <- which(handles$connecttype == "x")
                       theseAreConnected <-
                         which(handles$connection[polestring[indexOfClickedPoleOrZeroWithinEntireSet],] != 0)
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
                         for (i in (1:(length(theseAreConnected2)))) {
                           zeroconnect <- c(zeroconnect,
                                            which(zerostring ==
                                                    theseAreConnected2(i)))
                         }
                       }
                       else {
                         zeroconnect <- NULL
                       }
                       if (pracma::isempty(zeroconnect)) {
                         if ((x ^ 2 + y ^ 2) != 1) {
                           x <- x + 1e-06
                         }
                         subscriptOfClickedPoleOrZero <-
                           handles$subscriptOfClickedPoleOrZero
                         handles$poleloc[subscriptOfClickedPoleOrZero] <- x +
                           y * (0 + 1i)
                         tempstring <- handles$poleloc
                         tempstring <-
                           c(tempstring[1:(subscriptOfClickedPoleOrZero -
                                             1)], x + y * (0 + 1i), tempstring[seq(
                                               from = (subscriptOfClickedPoleOrZero +
                                                         1),
                                               to = length(tempstring)
                                             )])
                         handles$selectedpole <- list(XData = x, YData = y)
                         handles$selectedzero <- list(XData = NULL, YData = NULL)
                         if (!pracma::isempty(outputParametersFromStartdragToMovepez_poleconnect)) {
                           p1 <- outputParametersFromStartdragToMovepez_poleconnect[1]
                           handles$poleloc[p1] <- x - y * (0 + 1i)
                           tempstring <-
                             c(tempstring[1:(p1 - 1)], x - y * (0 + 1i),
                               tempstring[seq(from = (p1 + 1),
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
                         if (Mod(Im((1 / c(
                           handles$poleloc[handles$subscriptOfClickedPoleOrZero]
                         ))) -
                         Im(handles$zeroloc[outputParametersFromStartdragToMovepez_zeroconnect[1]])) <
                         handlestol) {
                           outputParametersFromStartdragToMovepez_type <- 1
                         }
                         else {
                           outputParametersFromStartdragToMovepez_type <- 0
                         }
                         if ((x ^ 2 + y ^ 2) != 1) {
                           x <- x + 1e-06
                         }
                         tempx <- Re(1 / c(x + y * (0 + 1i)))
                         tempy <- Im(1 / c(x + y * (0 + 1i)))
                         subscriptOfClickedPoleOrZero <-
                           handles$subscriptOfClickedPoleOrZero
                         handles$poleloc[subscriptOfClickedPoleOrZero] <- x +
                           y * (0 + 1i)
                         tempstring <- handles$poleloc
                         tempstring <-
                           c(tempstring[1:(subscriptOfClickedPoleOrZero -
                                             1)], x + y * (0 + 1i), tempstring[seq(
                                               from = (subscriptOfClickedPoleOrZero +
                                                         1),
                                               to = length(tempstring)
                                             )])
                         handles$selectedpole <- list(XData = x, YData = y)
                         if (!pracma::isempty(outputParametersFromStartdragToMovepez_poleconnect)) {
                           p1 <- outputParametersFromStartdragToMovepez_poleconnect[1]
                           handles$poleloc[p1] <- x - y * (0 + 1i)
                           tempstring <-
                             c(tempstring[1:(p1 - 1)], x - y * (0 + 1i),
                               tempstring[seq(from = (p1 + 1),
                                              to = length(tempstring))])
                           handles$selectedpole <-
                             list(XData = c(x, x), YData = c(y,-y))
                         }
                         updateSelectInput(session, inputId = "listbox_pole",
                                           choices = tempstring)
                         tempstring <- handles$zeroloc
                         if (outputParametersFromStartdragToMovepez_type ==
                             1) {
                           z1 <- outputParametersFromStartdragToMovepez_zeroconnect[1]
                           handles$zeroloc[z1] <- tempx + tempy * (0 + 1i)
                           tempstring <-
                             c(tempstring[1:(z1 - 1)], tempx + tempy *
                                 (0 + 1i), tempstring[seq(from = (z1 + 1),
                                                          to = length(tempstring))])
                           handles$selectedzero <-
                             list(XData = tempx, YData = tempy)
                           if (length(outputParametersFromStartdragToMovepez_zeroconnect) >
                               1) {
                             z2 <- outputParametersFromStartdragToMovepez_zeroconnect[2]
                             handles$zeroloc[z2] <- tempx - tempy * (0 + 1i)
                             tempstring <- c(tempstring[1:(z2 - 1)], tempx -
                                               tempy * (0 + 1i), tempstring[seq(from = (z2 + 1),
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
                             c(tempstring[1:(z1 - 1)], tempx - tempy *
                                 (0 + 1i), tempstring[seq(from = (z1 + 1),
                                                          to = length(tempstring))])
                           handles$selectedzero <-
                             list(XData = tempx, YData = -tempy)
                           if (length(outputParametersFromStartdragToMovepez_zeroconnect) >
                               1) {
                             z2 <- outputParametersFromStartdragToMovepez_zeroconnect[2]
                             handles$zeroloc[z2] <- tempx + tempy * (0 + 1i)
                             tempstring <- c(tempstring[1:(z2 - 1)], tempx +
                                               tempy * (0 + 1i), tempstring[seq(from = (z2 + 1),
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
author: ', rmarkdown::metadata$author, '
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
  n: NA
  inputcommonFilters: NA
  handlesb: NA
  handlesa: NA
  zeroloc: NA
  poleloc: NA
  normalizedMagPlotAmplitude: NA
  ForegroundColor: NA
  LineWidth: NA
  FFTshifted: NA
  unwrapPhase: NA
  twosidedFFT: NA
  samplingfreq: NA
  showPhaseOnMagPlot: NA
  logarithmicFreqAxis: NA
  grcolor: NA
  freqaxisunits: NA
  checkboxRAY: NA
  slider1: NA
  secondaryaxis: NA
  logarithmicMagPlotAmplitude: NA
  BackgroundColor: NA
  edit_gain: NA
  showLegend: NA
  showMaxMinsOnMagPlot: NA
  magnminimumsa: NA
  magnminimumsf: NA
  magnmaximumsa: NA
  magnmaximumsf: NA
  zoomlimXpassband: NA
  zoomlimYpassband: NA
  zoomlimXstopband: NA
  zoomlimYstopband: NA
  includeSourceCode: NA
  appwd: NA
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
# any parameter within the header cannot be `NULL`, so we used a default 
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

A plot of `r params$n` quantity of 2D normally-distributed random-points.

\```{r, fig.cap="This is the Plot."}
plot(rnorm(params$n), rnorm(params$n))
grid(); abline(h=0); abline(v=0); box(which="figure")
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
           nups = 1,
           ndowns = nups,
           zero = "0",
           peakpat = NULL,
           minpeakheight = -Inf,
           minpeakdistance = 1,
           threshold = 0,
           npeaks = 0,
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
    if (rc[1] < 0)
      return(NULL)
    x1 <- rc
    x2 <- rc + attr(rc, "match.length")
    attributes(x1) <- NULL
    attributes(x2) <- NULL
    n <- length(x1)
    xv <- xp <- numeric(n)
    for (i in 1:n) {
      xp[i] <- which.max(x[x1[i]:x2[i]]) + x1[i] - 1
      xv[i] <- x[xp[i]]
    }
    inds <-
      which(xv >= minpeakheight & xv - pmax(x[x1], x[x2]) >= threshold)
    X <- cbind(xv[inds], xp[inds], x1[inds], x2[inds])
    if (minpeakdistance < 1)
      warning("Handling `minpeakdistance < 1`` is logically not possible.")
    if (sortstr || minpeakdistance > 1) {
      sl <- sort.list(X[, 1], na.last = NA, decreasing = TRUE)
      X <- X[sl, , drop = FALSE]
    }
    if (length(X) == 0)
      return(c())
    if (minpeakdistance > 1) {
      no_peaks <- nrow(X)
      badpeaks <- rep(FALSE, no_peaks)
      for (i in 1:no_peaks) {
        ipos <- X[i, 2]
        if (!badpeaks[i]) {
          dpos <- abs(ipos - X[, 2])
          badpeaks <- badpeaks | (dpos > 0 & dpos < minpeakdistance)
        }
      }
      X <- X[!badpeaks,]
    }
    if (is.vector(X)) {
      if (npeaks > 0 && npeaks < length(X) / 4) {
        X <- X[1:npeaks, , drop = FALSE]
      }
    }
    else {
      if (npeaks > 0 && npeaks < nrow(X)) {
        X <- X[1:npeaks, , drop = FALSE]
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
      20 * log10(1 / params$edit_gain)
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
      1 / params$edit_gain
    })
  },
  log = if (params$logarithmicFreqAxis) {
    "x"
  }
  else {
    ""
  },
  xlim = if (params$logarithmicFreqAxis) {
    c(max(0.1, 1e-04 * params$samplingfreq / 2),
      params$samplingfreq / 2)
  }
  else {
    c(if (params$twosidedFFT) {
      -params$samplingfreq / 2
    } else {
      0
    }, if (params$FFTshifted) {
      params$samplingfreq / 2
    } else {
      if (params$twosidedFFT) {
        params$samplingfreq / 2
      } else {
        2 * params$samplingfreq / 2
      }
    })
  },
  ylim = if (params$logarithmicMagPlotAmplitude) {
    c(max(-130, min(-20, 20 * log10(
      min(Mod(rv$h), na.rm = TRUE) / (if (params$normalizedMagPlotAmplitude) {
        max(Mod(rv$h), na.rm = TRUE)
      } else {
        1 / params$edit_gain
      })
    ))), max(0, min(120, 20 * log10(
      max(Mod(rv$h), na.rm = TRUE) / (if (params$normalizedMagPlotAmplitude) {
        max(Mod(rv$h), na.rm = TRUE)
      } else {
        1 / params$edit_gain
      })
    ), na.rm = TRUE)))
  }
  else {
    c(0, max(1, Mod(rv$h) / (if (params$normalizedMagPlotAmplitude) {
      max(Mod(rv$h), na.rm = TRUE)
    } else {
      1 / params$edit_gain
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
    expression(2 * omega ~ ~  ~  ~ "[rads]")
  }
  else if (params$freqaxisunits == "zero2fs") {
    if (params$twosidedFFT)
      expression(-f[s] %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
    else
      expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
  }
  else if (params$freqaxisunits == "zero2fsby2") {
    if ((!params$twosidedFFT) &&
        (!params$FFTshifted) && (!params$logarithmicFreqAxis)) {
      expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
    }
    else {
      expression(0 %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
    }
  }
  else if (params$freqaxisunits == "zero2fmax") {
    expression(0 %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
  }
  else if (params$freqaxisunits == "zero2fmaxby2") {
    if (params$twosidedFFT)
      expression(-f[max] / 2 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
    else
      expression(0 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
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
            j * omega
          }
        )), "|"), " / max [dB]"))
      }
      else {
        expression(paste(group("|", bold(H(
          e ^ {
            j * omega
          }
        )), "|")), " / max")
      }
    }
    else {
      if (params$logarithmicMagPlotAmplitude) {
        expression(paste(group("|", bold(H(
          e ^ {
            j * omega
          }
        )), "|"), "  [dB]"))
      }
      else {
        expression(group("|", bold(H(e ^ {
          j * omega
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
abline(h = 0, col = "black")
abline(v = 0, col = "black")
if (params$secondaryaxis) {
  if ((params$freqaxisunits == "zero2pi") || (params$freqaxisunits ==
                                             "zero22pi") ||
      (params$freqaxisunits == "zero2piby2")) {
    axis(
      side = 3,
      at=pi*c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
      labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
      tick = TRUE,
      pos = par("usr")[4],
      outer = FALSE,
      col = params$grcolor,
      cex.axis = 0.8,
      mgp = c(-2.5,-1,0),
      xlab = "pi-based"
    )
  }
  if ((params$freqaxisunits == "zero2one") ||
      (params$freqaxisunits ==
       "zero2half")) {
    axis(
      side = 3,
      at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
      labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
      tick = TRUE,
      pos = par("usr")[4],
      outer = FALSE,
      col = params$grcolor,
      cex.axis = 0.8,
      mgp = c(-2.5,-1,0)
    )
  }
  if ((params$freqaxisunits == "zero2fs") ||
      (params$freqaxisunits ==
       "zero2fmax") ||
      (params$freqaxisunits == "zero2fmaxby2")) {
    axis(
      side = 3,
      at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6
      ) * params$samplingfreq / 2,
      labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
      tick = TRUE,
      line = -0.4,
      pos = par("usr")[4],
      outer = FALSE,
      col = params$grcolor,
      cex.axis = 0.8,
      mgp = c(-2.5,-1, 0)
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
    lwd = params$LineWidth * 2 / 3,
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
      minpeakdistance = (length(rv$h) / 100) / 2,
      threshold = 0.001,
      npeaks = 100
    )
  xmins <-
    myfindpeaks(
      -Mod(rv$h),
      minpeakdistance = (length(rv$h) / 100) / 2,
      threshold = 0.001,
      npeaks = 100
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
                                                             )) - 0.01))
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
            20 * log10(1 / params$edit_gain)
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
          indxs <- which(params$magnmaximumsa < (20 *log10(max(Mod(rv$h), na.rm = TRUE
                                                             )) - 0.01))
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
            20 * log10(1 / params$edit_gain)
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
    #   lwd = params$LineWidth * 2 / 3,
    #   pch = 24,
    #   cex = 2,
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
    #   pch = 25,
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
     20 * log10(1 / params$edit_gain)
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
     1 / params$edit_gain
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
   expression(2 * omega ~ ~  ~  ~ "[rads]")
 }
 else if (params$freqaxisunits == "zero2fs") {
   if (params$twosidedFFT)
     expression(-f[s] %->% ~
                  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
   else
     expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
 }
 else if (params$freqaxisunits == "zero2fsby2") {
   if ((!params$twosidedFFT) &&
       (!params$FFTshifted) && (!params$logarithmicFreqAxis)) {
     expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
   }
   else {
     expression(0 %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
   }
 }
 else if (params$freqaxisunits == "zero2fmax") {
   expression(0 %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
 }
 else if (params$freqaxisunits == "zero2fmaxby2") {
   if (params$twosidedFFT)
     expression(-f[max] /
                  2 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
   else
     expression(0 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
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
           j * omega
         }
       )), "|"), " / max [dB]"))
     }
     else {
       expression(paste(group("|", bold(H(
         e ^ {
           j * omega
         }
       )), "|")), " / max")
     }
   }
   else {
     if (params$logarithmicMagPlotAmplitude) {
       expression(paste(group("|", bold(H(
         e ^ {
           j * omega
         }
       )), "|"), "  [dB]"))
     }
     else {
       expression(group("|", bold(H(e ^ {
         j * omega
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
abline(h = 0, col = "black")
abline(v = 0, col = "black")
if (params$secondaryaxis) {
 if ((params$freqaxisunits == "zero2pi") || (params$freqaxisunits ==
                                            "zero22pi") ||
     (params$freqaxisunits == "zero2piby2")) {
   axis(
     side = 3,
     at = pi * c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
     labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
     tick = TRUE,
     pos = par("usr")[4],
     outer = FALSE,
     col = params$grcolor,
     cex.axis = 0.8,
     mgp = c(-2.5,-1, 0),
     xlab = "pi-based"
   )
 }
 if ((params$freqaxisunits == "zero2one") ||
     (params$freqaxisunits == "zero2half")
 ) {
   axis(
     side = 3,
     at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
     labels = expression(-6*pi,-11*pi/2,       -5 * pi,       -9 * pi / 2,       -4 * pi,       -7 * pi / 2,-3 * pi,       -5 * pi / 2,       -2 * pi,       -3 * pi / 2,       -pi,       -pi / 2,-pi / 3,       -pi / 5,       pi / 5,       pi / 3,       pi / 2,       pi,       3 * pi / 2,       2 * pi,       5 * pi /         2,       3 * pi,       7 * pi / 2,       4 * pi,       9 * pi / 2,       5 * pi,       11 * pi /         2,       6 * pi     ),
     tick = TRUE,
     pos = par("usr")[4],
     outer = FALSE,
     col = params$grcolor,
     cex.axis = 0.8,
     mgp = c(-2.5,-1, 0)
   )
 }
 if ((params$freqaxisunits == "zero2fs") ||
     (params$freqaxisunits == "zero2fmax") ||
     (params$freqaxisunits == "zero2fmaxby2")
 ) {
   axis(
     side = 3,
     at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6
     ) * params$samplingfreq / 2,
     labels = expression(-6*pi,-11*pi/2,       -5 * pi,       -9 *         pi /         2,       -4 * pi,       -7 * pi / 2,       -3 * pi,       -5 * pi / 2,       -2 *         pi,       -3 * pi / 2,       -pi,       -pi / 2,       -pi / 3,       -pi / 5,       pi / 5,       pi / 3,       pi /         2,       pi,       3 * pi / 2,       2 * pi,       5 * pi / 2,       3 * pi,       7 *         pi /         2,       4 * pi,       9 * pi / 2,       5 * pi,       11 * pi / 2,       6 *         pi     ),
     tick = TRUE,
     line = -0.4,
     pos = par("usr")[4],
     outer = FALSE,
     col = params$grcolor,
     cex.axis = 0.8,
     mgp = c(-2.5,-1, 0)
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
   lwd = params$LineWidth * 2 / 3,
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
     0
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
     1 / params$edit_gain
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
   expression(2 * omega ~ ~  ~  ~ "[rads]")
 }
 else if (params$freqaxisunits == "zero2fs") {
   if (params$twosidedFFT)
     expression(-f[s] %->% ~
                  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
   else
     expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
 }
 else if (params$freqaxisunits == "zero2fsby2") {
   if ((!params$twosidedFFT) &&
       (!params$FFTshifted) && (!params$logarithmicFreqAxis)) {
     expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
   }
   else {
     expression(0 %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
   }
 }
 else if (params$freqaxisunits == "zero2fmax") {
   expression(0 %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
 }
 else if (params$freqaxisunits == "zero2fmaxby2") {
   if (params$twosidedFFT)
     expression(-f[max] /
                  2 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
   else
     expression(0 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
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
           j * omega
         }
       )), "|"), " / max [dB]"))
     }
     else {
       expression(paste(group("|", bold(H(
         e ^ {
           j * omega
         }
       )), "|")), " / max")
     }
   }
   else {
     if (params$logarithmicMagPlotAmplitude) {
       expression(paste(group("|", bold(H(
         e ^ {
           j * omega
         }
       )), "|"), "  [dB]"))
     }
     else {
       expression(group("|", bold(H(e ^ {
         j * omega
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
abline(h = 0, col = "black")
abline(v = 0, col = "black")
if (params$secondaryaxis) {
 if ((params$freqaxisunits == "zero2pi") || 
     (params$freqaxisunits == "zero22pi") ||
     (params$freqaxisunits == "zero2piby2")) {
   axis(
     side = 3,
     at = pi * c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
     labels = expression(-6*pi,-11*pi/2,       -5 * pi,       -9 * pi / 2,       -4 * pi,       -7 * pi / 2,-3 * pi,       -5 * pi / 2,       -2 * pi,       -3 * pi / 2,       -pi,       -pi / 2,-pi / 3,       -pi / 5,       pi / 5,       pi / 3,       pi / 2,       pi,       3 * pi / 2,       2 * pi,       5 * pi /         2,       3 * pi,       7 * pi / 2,       4 * pi,       9 * pi / 2,       5 * pi,       11 * pi /         2,       6 * pi     ),
     tick = TRUE,
     pos = par("usr")[4],
     outer = FALSE,
     col = params$grcolor,
     cex.axis = 0.8,
     mgp = c(-2.5,-1,0),
     xlab = "pi-based"
   )
 }
 if ((params$freqaxisunits == "zero2one") ||
     (params$freqaxisunits == "zero2half")) {
   axis(
     side = 3,
     at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
     labels = expression(-6*pi,-11*pi/2,       -5 * pi,       -9 * pi / 2,       -4 * pi,       -7 * pi / 2,-3 * pi,       -5 * pi / 2,       -2 * pi,       -3 * pi / 2,       -pi,       -pi / 2,-pi / 3,       -pi / 5,       pi / 5,       pi / 3,       pi / 2,       pi,       3 * pi / 2,       2 * pi,       5 * pi /         2,       3 * pi,       7 * pi / 2,       4 * pi,       9 * pi / 2,       5 * pi,       11 * pi /         2,       6 * pi     ),
     tick = TRUE,
     pos = par("usr")[4],
     outer = FALSE,
     col = params$grcolor,
     cex.axis = 0.8,
     mgp = c(-2.5,-1,0)
   )
 }
 if ((params$freqaxisunits == "zero2fs") ||
     (params$freqaxisunits == "zero2fmax") ||
     (params$freqaxisunits == "zero2fmaxby2")) {
   axis(
     side = 3,
     at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6
     ) * params$samplingfreq / 2,
     labels = expression(-6*pi,-11*pi/2,       -5 * pi,       -9 *         pi /         2,       -4 * pi,       -7 * pi / 2,       -3 * pi,       -5 * pi / 2,       -2 *         pi,       -3 * pi / 2,       -pi,       -pi / 2,       -pi / 3,       -pi / 5,       pi / 5,       pi / 3,       pi /         2,       pi,       3 * pi / 2,       2 * pi,       5 * pi / 2,       3 * pi,       7 *         pi /         2,       4 * pi,       9 * pi / 2,       5 * pi,       11 * pi / 2,       6 *         pi     ),
     tick = TRUE,
     line = -0.4,
     pos = par("usr")[4],
     outer = FALSE,
     col = params$grcolor,
     cex.axis = 0.8,
     mgp = c(-2.5,-1, 0)
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
   lwd = params$LineWidth * 2 / 3,
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
kable(params$handlesb, caption = "b Coefficients (moving-average MA)")
\```

\```{r aCoefficients}
kable(params$handlesa, caption = "a Coefficients (autoregressive AR)")
\```

\```{r MinimumValues}
kable(cbind(minfreqs=params$magnminimumsf,minamplitudes=params$magnminimumsa), 
      caption = "Minimum Values")
\```

\```{r MaximumValues}
kable(cbind(maxfreqs=params$magnmaximumsf,maxamplitudes=params$magnmaximumsa), 
      caption = "Maximum Values")
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
    vwidth = 1366, delay = 90)
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
        params <-
          list( # pass the parameters for TEMPLATE report.Rmd ----
            n = input$slider,
            inputcommonFilters = input$commonFilters,
            handlesb = handlesb(),
            handlesa = handlesa(),
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
                               1:length(
                                 handles$poleloc
                               )), paste0("z", 1:length(
                                 handles$zeroloc
                               ))))
    point <- nearPoints(
      df = Mydf,
      coordinfo = input$pzplot_hover,
      xvar = "xReal",
      yvar = "yImag",
      threshold = 7,
      maxpoints = 1,
      addDist = TRUE
    )
    if (!(handles$inDragMode) && (NROW(point) == 0))
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
        round(point$xReal, 2),
        "</b>",
        ", ",
        "<b>",
        round(point$yImag, 2),
        "</b>]"
      )
    ))
  })
  
  # output$hover_info2 - cursor-coordinates, magnitude-plot ----
  output$hover_info2 <- renderUI({
    req(handles$magnmaximumsf, handles$magnminimumsa)
    Myhover <- input$magplot_hover
    if ((length(handles$magnmaximumsa) > 0) ||
        (length(handles$magnminimumsa) >
         0)) {
      Mydf <- as.data.frame(cbind(
        frq = c(handles$magnmaximumsf,
                handles$magnminimumsf),
        magn = c(handles$magnmaximumsa,
                 handles$magnminimumsa)
      ), row.names = c(paste0(
        "s", 1:length(handles$magnmaximumsf)
      ),
      paste0(
        "m", 1:length(handles$magnminimumsf)
      )))
      point <-
        nearPoints(
          df = Mydf,
          coordinfo = input$magplot_hover,
          xvar = "frq",
          yvar = "magn",
          threshold = 7,
          maxpoints = 1,
          addDist = TRUE
        )
      if (NROW(point) == 0)
        return(NULL)
      left_pct <- (if (input$logarithmicFreqAxis) {
        0.001 * log10(Myhover$x)
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
          paste0(top_px +
                   2, "px;")
        )
       # http://www.77dev.com/2016/03/custom-interactive-csshtml-tooltips.html
      wellPanel(style = style, class = "well-sm", HTML(
        paste(
          "<b>",
          rownames(point),
          "</b>: [",
          "<b>",
          round(point$frq, 2),
          "</b>",
          ", ",
          "<b>",
          round(point$magn, 2),
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
            ncolumns = 1,
            append = FALSE,
            sep = " "
          )
        }
        else if (input$textbinaryformatExport == "bin") {
          zz <- file(paste0(input$filenameExport, ".bin"), open = "wb")
          writeBin(c(
            poleloc = handles$poleloc,
            0,
            zeroloc = handles$zeroloc
          ),
          con = zz)
          close(zz)
          zz <- file(paste0(input$filenameExport, ".bin"), open = "rb")
          str(zz)
          print(readBin(
            con = zz,
            what = complex(),
            n = 10
          ))
          close(zz)
        }
        else if (input$textbinaryformatExport == "RData") {
          mypolescopy <- handles$poleloc
          myzeroscopy <- handles$zeroloc
          save(myzeroscopy, mypolescopy, file = "rpolezerolocs.RData")
          load(file = paste0(input$filenameExport, ".RData"),
               verbose = TRUE)
        }
        else if (input$textbinaryformatExport == "mat") {
          mypolescopy <- handles$poleloc
          myzeroscopy <- handles$zeroloc
          R.matlab::writeMat(
            paste0(input$filenameExport, ".mat"),
            mypolescopy = mypolescopy,
            myzeroscopy = myzeroscopy,
            verbose = TRUE
          )
        }
        else if (input$textbinaryformatExport == "yml") {
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
      runif(5)
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
          pointsize = 12,
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
                 filtb[Im(filtb) > 1e+12] <- Re(filtb) + j * 1e+12
                 filtb[Im(filtb) < -1e+12] <- Re(filtb) - j * 1e+12
                 filta[Im(filta) > 1e+12] <- Re(filtb) + j * 1e+12
                 filta[Im(filta) < -1e+12] <- Re(filtb) - j * 1e+12
                 rv <-
                   signal::freqz(
                     filtb,
                     filta,
                     region = "whole",
                     n = 2 ^ 20,
                     Fs = 2 *
                       pi * input$samplingfreq / 2
                   )
                 updateNumericInput(session, inputId = "edit_gain", value = 1 / max(Re(rv$h)))
                 updateCheckboxInput(session, inputId = "normalizedMagPlotAmplitude",
                                     value = TRUE)
               })
  
  # handlesa reactive ----
  handlesa <- reactive({
    updateSelectInput(session,
                      inputId = "listbox_a",
                      choices = round(pracma::Poly(handles$poleloc), 6))
    shinyjs::disable(id = "listbox_a")
    rawcalc <- pracma::Poly(handles$poleloc)
    rawcalc
  })

  # handlesb reactive ----
  handlesb <- reactive({
    updateSelectInput(session,
                      inputId = "listbox_b",
                      choices = round(pracma::Poly(handles$zeroloc),
                                      6))
    shinyjs::disable(id = "listbox_b")
    rawcalc <- pracma::Poly(handles$zeroloc)
    rawcalc
  })
  
  # handleshz reactive ----
  handleshz <- reactive({
    N <- 512
    pt <- input$maxLengthImpulseResponse
    pracma::fftshift((fft(c(
      input$edit_gain * handlesb(), rep(0, N -
                                          length(input$edit_gain * handlesb()))
    ))) / c(fft(c(
      handlesa(),
      rep(0, N - length(handlesa()))
    ))))
  })
  
  hnimag <- reactive({
    pt <- input$maxLengthImpulseResponse
    if ((length(handlesb()) >= 2) && (length(handlesa()) >= 2)) {
      temphnimag <-
        signal::filter(c(handlesb()[1], Im(handlesb()[2:length(handlesb())])),
                       c(handlesa()[1], Im(handlesa()[2:length(handlesa())])),
                       c(1, rep(0, times = (pt - 1)))) # impulse
    }
    else if (length(handlesa()) >= 2) {
      temphnimag <-
        signal::filter(handlesb(), c(handlesa()[1], Im(handlesa()[2:length(handlesa())])),
                       c(1, rep(0, times = (pt - 1)))) # impulse
    }
    else {
      temphnimag <- signal::filter(handlesb(), handlesa(), c(1, rep(0,
                                                                    times = (pt - 1))))
    }
    if (length(temphnimag) < 2) {
      temphnimag <- temphnimag * 0
    }
    else if (max(abs(temphnimag[2:length(temphnimag)])) > 0) {
      temphnimag <- temphnimag * 0
      temphnimag[2] <- (0 + 1i) * Im(handles$poleloc[1])
    }
    temphnimag
  })
  
  hnimagu <- reactive({
    pt <- input$maxLengthImpulseResponse
    if ((length(handlesb()) >= 2) && (length(handlesa()) >= 2)) {
      temphnimag <-
        signal::filter(c(handlesb()[1], Im(handlesb()[2:length(handlesb())])),
                       c(handlesa()[1], Im(handlesa()[2:length(handlesa())])),
                       c(1, rep(1, times = (pt - 1)))) # step
    }
    else if (length(handlesa()) >= 2) {
      temphnimag <-
        signal::filter(handlesb(), c(handlesa()[1], Im(handlesa()[2:length(handlesa())])),
                       c(1, rep(1, times = (pt - 1)))) # step
    }
    else {
      temphnimag <- signal::filter(handlesb(), handlesa(), c(1, rep(0,
                                                                    times = (pt - 1))))
    }
    if (length(temphnimag) < 2) {
      temphnimag <- temphnimag * 0
    }
    else if (max(abs(temphnimag[2:length(temphnimag)])) > 0) {
      temphnimag <- temphnimag * 0
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
    #   n = pt - 1
    # )
    signal::filter( # real-parts only - imaginary-parts are "discarded in coercion"
      filt = input$edit_gain * handlesb(),
      a = handlesa(),
      x = c(1, rep(0, times = (pt - 1))) # impulse
    )
  })
  
  # handleshnu reactive ----
  handleshnu <- reactive({
    pt <- input$maxLengthImpulseResponse
    # signal::impz(
    #   filt = input$edit_gain * handlesb(),
    #   a = handlesa(),
    #   n = pt - 1
    # )
    signal::filter(
      filt = input$edit_gain * handlesb(),
      a = handlesa(),
      x = c(1, rep(1, times = (pt - 1))) # unit-step
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
      nc <- 100
      cc <- 1 * exp((0 + (0 + 1i)) * 2 * pi * c(0:(nc - 1)) / (nc - 1))
      usr1 <- par("usr")[1]
      usr2 <- par("usr")[2]
      usr3 <- par("usr")[3]
      usr4 <- par("usr")[4]
      xlims <-
        c(max(-5, 1.05 * min(
          Re(handles$poleloc),
          Re(handles$zeroloc),
          input$zoomlimX[1],
          na.rm = TRUE
        ), na.rm = TRUE),
        min(5, 1.05 *
              max(
                Re(handles$poleloc),
                Re(handles$zeroloc),
                input$zoomlimX[2],
                na.rm = TRUE
              ), na.rm = TRUE))
      ylims <-
        c(max(-5, 1.05 * min(
          Im(handles$poleloc),
          Im(handles$zeroloc),
          input$zoomlimY[1],
          na.rm = TRUE
        ), na.rm = TRUE),
        min(5, 1.05 *
              max(
                Im(handles$poleloc),
                Im(handles$zeroloc),
                input$zoomlimY[2],
                na.rm = TRUE
              ), na.rm = TRUE))
      plot(
        1 * cc,
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
        las = 0,
        asp = 1
      )
      if (input$checkboxRAY) {
        # must determine quadrant http://www.intmath.com/trigonometric-functions/5-signs-of-trigonometric-functions.php
        switch(3 + sign(sin(
          input$slider1 * pi / (input$samplingfreq / 2)
        )) +
          sign(cos(
            input$slider1 * pi / (input$samplingfreq / 2)
          )),
        {
          rayX <- c(0,-10)
          rayY <-
            c(0,-10 * tan(input$slider1 * pi / (input$samplingfreq / 2)))
        },
        if (abs(sin(
          input$slider1 * pi / (input$samplingfreq / 2)
        )) <=
        2 * eps) {
          rayX <- c(0,-10)
          rayY <- c(0, 0)
        } else {
          rayX <- c(0, 0)
          rayY <- c(0,-10)
        },
        if (sin(input$slider1 * pi / (input$samplingfreq / 2)) > 0) {
          rayX <- c(0,-10)
          rayY <-
            c(0,-10 * tan(input$slider1 * pi / (input$samplingfreq / 2)))
        } else {
          rayX <- c(0, 10)
          rayY <-
            c(0, 10 * tan(input$slider1 * pi / (input$samplingfreq / 2)))
        },
        if (abs(sin(
          input$slider1 * pi / (input$samplingfreq / 2)
        )) <=
        2 * eps) {
          rayX <- c(0, 10)
          rayY <- c(0, 0)
        } else {
          rayX <- c(0, 0)
          rayY <- c(0, 10)
        },
        {
          rayX <- c(0, 10)
          rayY <-
            c(0, 10 * tan(input$slider1 * pi / (input$samplingfreq / 2)))
        })
        if (abs(rayX[1]) < eps)
          rayX[1] <- 0
        if (abs(rayX[2]) < eps)
          rayX[2] <- 0
        if (abs(rayY[1]) < eps)
          rayY[1] <- 0
        if (abs(rayY[2]) < eps)
          rayY[2] <- 0
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
        abline(h = 0, col = "magenta")
        abline(v = 0, col = "magenta")
      }
      if (max(Mod(handles$poleloc)) > (1 + 1e-04)) {
        box(
          which = "plot",
          col = MyColourForUnstableSystem,
          lwd = 3 *
            input$LineWidth,
          lty = "dashed"
        )
        box(
          which = "inner",
          col = MyColourForUnstableSystem,
          lwd = 3 *
            input$LineWidth,
          lty = "dotted"
        )
        symbols(
          0,
          0,
          circles = 0.99,
          inches = FALSE,
          bg = MyColourForUnstableSystem,
          add = TRUE
        )
        grid(col = MyColourForUnstableSystem)
        abline(h = 0, col = MyColourForUnstableSystem)
        abline(v = 0, col = MyColourForUnstableSystem)
      }
      rmin <- 0.2
      rstep <- rmin
      rmax <- 10 - rstep
      for (r in (seq(rmin, rmax, by = rstep))) {
        lines(r * cc, lty = "dotted", col = if (input$polargrid) {input$grcolor} else {"transparent"})
      }
      lines(
        1 * cc,
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
      ell <- seq(rmin, 10, by = 1 / nc)
      tmin <- pi / 12
      for (t in (seq(tmin, 2 * pi, by = tmin))) {
        r <- cos(t) * ell + (0 + (0 + 1i)) * sin(t) * ell
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
                                                                        1)))
            }
            else if ((t > pi / 3) && (t < pi)) {
              substitute(paste(frac(MYVALUE1 * pi, MYVALUE2)),
                         list(
                           MYVALUE1 = sub("^1$",
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
              substitute(paste(MYVALUE, pi), list(MYVALUE = round((t / pi), 2)))
            }
          },
          col = if (input$polargrid) {input$grcolor} else {"transparent"},
          adj = c(0.5 - 0.73 * cos(t), 0.5 - 0.73 * sin(t))
        )
      }
      tmin <- pi / 8
      for (t in (seq(tmin, 2 * pi, by = tmin))) {
        r <- cos(t) * ell + (0 + (0 + 1i)) * sin(t) * ell
        lines(Re(r), Im(r), lty = "dotted", col = if (input$polargrid) {input$grcolor} else {"transparent"})
        text(
          cos(t),
          sin(t),
          if (input$degreesgrid) {
            round(t * 180 / pi)
          }
          else {
            if (t <= pi / 3) {
              substitute(paste(frac(pi, MYVALUE)), list(MYVALUE = round(pi/t, 1)))
            }
            else if ((t > pi / 3) && (t < pi)) {
              substitute(paste(frac(MYVALUE1 * pi, MYVALUE2)),
                         list(
                           MYVALUE1 = sub("^1$",
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
              substitute(paste(MYVALUE * pi), list(MYVALUE = round((t / pi), 2)))
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
        texton <- 1
        
        zeta <- seq(0.1,1, by=0.1)
        # wn_a <- pracma::linspace(0,2*pi,n=100)
        wn_a <- seq(0,2, length.out=100) * pi
        
        # plot the lines for z
        rv <- pracma::meshgrid(zeta,wn_a)
        zz <- rv$X
        wwn <- rv$Y
        
        s <- -zz * wwn + 1i*wwn * sqrt(1 - zz^2)
        z <- exp(s)
        
        # Do this so that we don't wrap around.
        # z(imag(z)<0) <- real(z(imag(z)<0));
        # We do this ugly `for` loop, so that we don't make an ugly dark-line
        # on the real-axis.
        for (q in 1:dim(z)[2]) {
          y <- z[,q]
          y[Im(y)<0] <- Re(y[which( Im(y)<0 ,1)])
          z[,q] <- y
        }
        
        # par(pty="s")
        matlines(
          Re(z), Im(z),
          # type="l",
          col=input$grcolor,
          lwd=input$LineWidth/2
          # ,xlim=c(-1,1)
          # ,ylim=c(-1,1)
          ,lty = "dotted"
        )
        matlines(
          Re(Conj(z)), Im(Conj(z)),
          col=input$grcolor,
          lty = "dotted",
          lwd=input$LineWidth/2
        )
        
        if (texton > 0) {
          # put in text labels for zeta
          for (q in 1:(dim(z)[2])) {
            loc <- z[pracma::ceil(dim(z)[1]/4),q]
            if (texton == 2) {
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
        
        # zeta_a <- pracma::linspace(0,1, n=50)
        zeta_a <- seq(0,1, length.out=50)
        wn <- seq(0,1, by=0.1) * pi
        
        # plot the lines for wn.
        rv <- pracma::meshgrid(zeta_a,wn)
        zz <- rv$X
        wwn <- rv$Y
        
        s <- -zz*wwn + 1i*wwn*sqrt(1-zz^2)
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
          for (q in 1:(dim(z)[1])) {
            if (texton == 2) {
              text(Re(z[q,1]),0.03+Im(z[q,1]),
                   # paste0('wn = ',10,pracma::num2str(wn[q]/pi),'*pi/T')
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
        abline(h = 0, col = "black")
        abline(v = 0, col = input$grcolor)

      }
      
      points(
        Re(handles$zeroloc),
        Im(handles$zeroloc),
        pch = 21,
        cex = input$LineWidth,
        lwd = input$LineWidth,
        col = input$ForegroundColorZeros,
        bg = input$BackgroundColor
      )
      points(
        Re(handles$poleloc),
        Im(handles$poleloc),
        pch = 4,
        cex = input$LineWidth,
        lwd = input$LineWidth,
        col = input$ForegroundColorPoles
      )
      nCentralBalancePoints <-
        length(handles$poleloc) - length(handles$zeroloc)
      if ((length(handles$poleloc) == 1) &&
          (handles$poleloc[1] == 0)) {
        nCentralBalancePoints <- nCentralBalancePoints - 1
      }
      else if ((length(handles$zeroloc) == 1) &&
               (handles$zeroloc[1] ==
                0)) {
        nCentralBalancePoints <- nCentralBalancePoints + 1
      }
      if (nCentralBalancePoints > 0) {
        points(
          rep(0, nCentralBalancePoints),
          rep(0, nCentralBalancePoints),
          pch = 21,
          cex = input$LineWidth / 2,
          col = input$ForegroundColorZeros,
          bg = input$BackgroundColor,
          lwd = input$LineWidth
        )
        if (nCentralBalancePoints > 1) {
          text(
            0,
            0,
            labels = nCentralBalancePoints,
            pos = 1,
            offset = 0.8,
            cex = input$LineWidth / 2,
            col = input$ForegroundColorZeros,
            bg = input$BackgroundColor
          )
        }
      }
      else if (nCentralBalancePoints < 0) {
        points(
          rep(0,-nCentralBalancePoints),
          rep(0,-nCentralBalancePoints),
          pch = 4,
          cex = input$LineWidth / 2,
          col = input$ForegroundColorPoles,
          lwd = input$LineWidth
        )
        if (-nCentralBalancePoints > 1) {
          text(
            0,
            0,
            labels = -nCentralBalancePoints,
            pos = 3,
            offset = 0.7,
            cex = input$LineWidth / 2,
            col = input$ForegroundColorPoles
          )
        }
      }
      MyTableZeros <-
        matrix(data = 0,
               nrow = length(handles$zeroloc),
               ncol = 2)
      MyTableZeros[, 1] <- handles$zeroloc
      for (i in 1:NROW(MyTableZeros)) {
        MyTableZeros[i, 2] <- sum(MyTableZeros[, 1] == MyTableZeros[i,
                                                                    1])
      }
      MyTableZeros <- MyTableZeros[(Re(MyTableZeros[, 2]) > 1),]
      MyTableZeros <- unique(MyTableZeros)
      if (NROW(MyTableZeros) > 0) {
        text(
          Re(MyTableZeros[, 1]),
          Im(MyTableZeros[, 1]),
          labels = Re(MyTableZeros[,
                                   2]),
          pos = 1,
          offset = 0.8,
          cex = input$LineWidth / 2,
          col = input$ForegroundColorZeros,
          bg = input$BackgroundColor
        )
      }
      MyTablePoles <-
        matrix(data = 0,
               nrow = length(handles$poleloc),
               ncol = 2)
      MyTablePoles[, 1] <- handles$poleloc
      for (i in 1:NROW(MyTablePoles)) {
        MyTablePoles[i, 2] <- sum(MyTablePoles[, 1] == MyTablePoles[i,
                                                                    1])
      }
      MyTablePoles <- MyTablePoles[(Re(MyTablePoles[, 2]) > 1),]
      MyTablePoles <- unique(MyTablePoles)
      if (NROW(MyTablePoles) > 0) {
        text(
          Re(MyTablePoles[, 1]),
          Im(MyTablePoles[, 1]),
          labels = Re(MyTablePoles[,
                                   2]),
          pos = 3,
          offset = 0.7,
          cex = input$LineWidth / 2,
          col = input$ForegroundColorPoles
        )
      }
      MyFarAwayZeros <-
        matrix(data = 0,
               nrow = length(handles$zeroloc),
               ncol = 2)
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
        if (length(MyFarAwayZeros) == 2) {
          MyFarAwayZeros <- matrix(data = MyFarAwayZeros,
                                   ncol = 2,
                                   byrow = TRUE)
        }
        for (i in 1:NROW(MyFarAwayZeros)) {
          MyFarAwayZeros[i, 2] <-
            sum(MyFarAwayZeros[, 1] == MyFarAwayZeros[i,
                                                      1])
        }
        MyFarAwayZeros <- unique(MyFarAwayZeros)
        if (NROW(MyFarAwayZeros) > 0) {
          ReFarAwayZeros <- Re(MyFarAwayZeros[, 1])
          ImFarAwayZeros <- Im(MyFarAwayZeros[, 1])
          for (i in (1:NROW(MyFarAwayZeros))) {
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
            cottheta <- 1 / tantheta
            Myvec1 <-
              pi + c(
                atan2(usr3, usr1),
                atan2(usr3, 0),
                atan2(usr3,
                      usr2),
                atan2(0, usr2),
                atan2(usr4, usr2),
                atan2(usr4,
                      0),
                atan2(usr4, usr1),
                atan2(0, usr1)
              )
            mtext(
              side = switch(
                1 + findInterval(
                  x = pi + atan2(ImFarAwayZeros[i],
                                 ReFarAwayZeros[i]),
                  vec = Myvec1,
                  rightmost.closed = FALSE,
                  all.inside = FALSE,
                  left.open = FALSE
                ),
                2,                1,                1,                4,                4,                3,                3,                2,                2
              ),
              text = paste0("(O)", Re(MyFarAwayZeros[i,
                                                     2])),
              at = switch(
                1 + findInterval(
                  x = pi + atan2(ImFarAwayZeros[i],
                                 ReFarAwayZeros[i]),
                  vec = Myvec1,
                  rightmost.closed = FALSE,
                  all.inside = FALSE,
                  left.open = FALSE
                ),
                switch(
                  2 +
                    sign(ImFarAwayZeros[i]),
                  max(usr3, tantheta * usr1,
                      na.rm = TRUE),
                  0,
                  min(usr4, tantheta * usr1, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ReFarAwayZeros[i]),
                  max(usr1, cottheta *
                        usr3, na.rm = TRUE),
                  0,
                  min(usr2, cottheta * usr3,
                      na.rm = TRUE)
                ),
                switch(
                  2 + sign(ReFarAwayZeros[i]),
                  max(usr1, cottheta * usr3, na.rm = TRUE),
                  0,
                  min(usr2,
                      cottheta * usr3, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ImFarAwayZeros[i]),
                  max(usr3, tantheta * usr2, na.rm = TRUE),
                  0,
                  min(usr4,
                      tantheta * usr2, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ImFarAwayZeros[i]),
                  max(usr3, tantheta * usr2, na.rm = TRUE),
                  0,
                  min(usr4,
                      tantheta * usr2, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ReFarAwayZeros[i]),
                  max(usr1, cottheta * usr4, na.rm = TRUE),
                  0,
                  min(usr2,
                      cottheta * usr4, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ReFarAwayZeros[i]),
                  max(usr1, cottheta * usr4, na.rm = TRUE),
                  0,
                  min(usr2,
                      cottheta * usr4, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ImFarAwayZeros[i]),
                  max(usr3, tantheta * usr1, na.rm = TRUE),
                  0,
                  min(usr4,
                      tantheta * usr1, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ImFarAwayZeros[i]),
                  max(usr3, tantheta * usr1, na.rm = TRUE),
                  0,
                  min(usr4,
                      tantheta * usr1, na.rm = TRUE)
                )
              ),
              cex = input$LineWidth *
                2 / 3,
              col = input$ForegroundColorZeros,
              bg = input$BackgroundColor
            )
          }
        }
      }
      MyFarAwayPoles <-
        matrix(data = 0,
               nrow = length(handles$poleloc),
               ncol = 2)
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
          if (length(MyFarAwayPoles) == 2) {
            MyFarAwayPoles <- matrix(data = MyFarAwayPoles,
                                     ncol = 2,
                                     byrow = TRUE)
          }
        })
        if (NROW(MyFarAwayPoles) > 0) {
          for (i in 1:NROW(MyFarAwayPoles)) {
            isolate({
              MyFarAwayPoles[i, 2] <- sum(MyFarAwayPoles[, 1] ==
                                            MyFarAwayPoles[i, 1])
            })
          }
        }
        isolate({
          MyFarAwayPoles <- unique(MyFarAwayPoles)
        })
        if (NROW(MyFarAwayPoles) > 0) {
          ReFarAwayPoles <- Re(MyFarAwayPoles[, 1])
          ImFarAwayPoles <- Im(MyFarAwayPoles[, 1])
          for (i in (1:NROW(MyFarAwayPoles))) {
            if ((is.infinite(ImFarAwayPoles[i])) &&
                (is.infinite(ReFarAwayPoles[i]))) {
              tantheta <- sign(ImFarAwayPoles[i]) / sign(ReFarAwayPoles[i])
            }
            else {
              tantheta <- ImFarAwayPoles[i] / ReFarAwayPoles[i]
            }
            cottheta <- 1 / tantheta
            usr1 <- par("usr")[1]
            usr2 <- par("usr")[2]
            usr3 <- par("usr")[3]
            usr4 <- par("usr")[4]
            Myvec1 <-
              pi + c(
                atan2(usr3, usr1),
                atan2(usr3, 0),
                atan2(usr3,
                      usr2),
                atan2(0, usr2),
                atan2(usr4, usr2),
                atan2(usr4,
                      0),
                atan2(usr4, usr1),
                atan2(0, usr1)
              )
            mtext(
              side = switch(
                1 + findInterval(
                  x = pi + atan2(ImFarAwayPoles[i],
                                 ReFarAwayPoles[i]),
                  vec = Myvec1,
                  rightmost.closed = FALSE,
                  all.inside = FALSE,
                  left.open = FALSE
                ),
                2,1,1,4,4,3,3,2,2
              ),
              text = paste0("(X)", Re(MyFarAwayPoles[i, 2])),
              at = switch(
                1 + findInterval(
                  x = pi + atan2(ImFarAwayPoles[i],
                                 ReFarAwayPoles[i]),
                  vec = Myvec1,
                  rightmost.closed = FALSE,
                  all.inside = FALSE,
                  left.open = FALSE
                ),
                switch(
                  2 +
                    sign(ImFarAwayPoles[i]),
                  max(usr3, tantheta * usr1, na.rm = TRUE),
                  0,
                  min(usr4, tantheta * usr1, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ReFarAwayPoles[i]),
                  max(usr1, cottheta * usr3, na.rm = TRUE),
                  0,
                  min(usr2, cottheta * usr3, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ReFarAwayPoles[i]),
                  max(usr1, cottheta * usr3, na.rm = TRUE),
                  0,
                  min(usr2, cottheta * usr3, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ImFarAwayPoles[i]),
                  max(usr3, tantheta * usr2, na.rm = TRUE),
                  0,
                  min(usr4, tantheta * usr2, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ImFarAwayPoles[i]),
                  max(usr3, tantheta * usr2, na.rm = TRUE),
                  0,
                  min(usr4, tantheta * usr2, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ReFarAwayPoles[i]),
                  max(usr1, cottheta * usr4, na.rm = TRUE),
                  0,
                  min(usr2, cottheta * usr4, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ReFarAwayPoles[i]),
                  max(usr1, cottheta * usr4, na.rm = TRUE),
                  0,
                  min(usr2, cottheta * usr4, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ImFarAwayPoles[i]),
                  max(usr3, tantheta * usr1, na.rm = TRUE),
                  0,
                  min(usr4, tantheta * usr1, na.rm = TRUE)
                ),
                switch(
                  2 + sign(ImFarAwayPoles[i]),
                  max(usr3, tantheta * usr1, na.rm = TRUE),
                  0,
                  min(usr4, tantheta * usr1, na.rm = TRUE)
                )
              ),
              cex = input$LineWidth * 2 / 3,
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
                 nc <- 100
                 cc <-
                   1 * exp((0 + (0 + 1i)) * 2 * pi * c(0:(nc - 1)) / (nc - 1))
                 usr1 <- par("usr")[1]
                 usr2 <- par("usr")[2]
                 usr3 <- par("usr")[3]
                 usr4 <- par("usr")[4]
                 xlims <-
                   c(max(-5, 1.05 * min(
                     Re(handles$poleloc),
                     Re(handles$zeroloc),
                     input$zoomlimX[1],
                     na.rm = TRUE
                   ), na.rm = TRUE),
                   min(5,
                       1.05 * max(
                         Re(handles$poleloc),
                         Re(handles$zeroloc),
                         input$zoomlimX[2],
                         na.rm = TRUE
                       ), na.rm = TRUE))
                 ylims <-
                   c(max(-5, 1.05 * min(
                     Im(handles$poleloc),
                     Im(handles$zeroloc),
                     input$zoomlimY[1],
                     na.rm = TRUE
                   ), na.rm = TRUE),
                   min(5,
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
                   1 * cc,
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
                   main = "Pole-Zero Plot (Select desired-region within other plot)",
                   xlab = "Re(z)",
                   ylab = "Im(z) * j",
                   las = 0,
                   asp = 1
                 )
                 if (input$checkboxRAY) {
                   switch(3 + sign(sin(
                     input$slider1 * pi / (input$samplingfreq / 2)
                   )) +
                     sign(cos(
                       input$slider1 * pi / (input$samplingfreq / 2)
                     )),
                   {
                     rayX <- c(0,-10)
                     rayY <-
                       c(0,-10 * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   },
                   if (abs(sin(
                     input$slider1 * pi / (input$samplingfreq / 2)
                   )) <=
                   2 * eps) {
                     rayX <- c(0,-10)
                     rayY <- c(0, 0)
                   } else {
                     rayX <- c(0, 0)
                     rayY <-
                       c(0,-10)
                   },
                   if (sin(input$slider1 * pi / (input$samplingfreq / 2)) >
                       0) {
                     rayX <- c(0,-10)
                     rayY <-
                       c(0,-10 * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   } else {
                     rayX <- c(0, 10)
                     rayY <-
                       c(0, 10 * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   },
                   if (abs(sin(
                     input$slider1 * pi / (input$samplingfreq / 2)
                   )) <=
                   2 * eps) {
                     rayX <- c(0, 10)
                     rayY <- c(0, 0)
                   } else {
                     rayX <- c(0, 0)
                     rayY <- c(0, 10)
                   },
                   {
                     rayX <- c(0, 10)
                     rayY <-
                       c(0, 10 * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   })
                   if (abs(rayX[1]) < eps)
                     rayX[1] <- 0
                   if (abs(rayX[2]) < eps)
                     rayX[2] <- 0
                   if (abs(rayY[1]) < eps)
                     rayY[1] <- 0
                   if (abs(rayY[2]) < eps)
                     rayY[2] <- 0
                   lines(rayX, rayY, lwd = input$LineWidth, col = "magenta")
                 }
                 if (max(Mod(handles$poleloc)) > (1 + 1e-04)) {
                   box(
                     which = "plot",
                     col = MyColourForUnstableSystem,
                     lwd = 3 *
                       input$LineWidth,
                     lty = "dashed"
                   )
                   box(
                     which = "inner",
                     col = MyColourForUnstableSystem,
                     lwd = 3 *
                       input$LineWidth,
                     lty = "dotted"
                   )
                   symbols(
                     0,
                     0,
                     circles = 0.99,
                     inches = FALSE,
                     bg = MyColourForUnstableSystem,
                     add = TRUE
                   )
                   grid(col = MyColourForUnstableSystem)
                   abline(h = 0, col = MyColourForUnstableSystem)
                   abline(v = 0, col = MyColourForUnstableSystem)
                 }
                 rmin <- 0.2
                 rstep <- rmin
                 rmax <- 10 - rstep
                 for (r in (seq(rmin, rmax, by = rstep))) {
                   lines(r * cc, lty = "dotted", col = if (input$polargrid) {input$grcolor} else {"transparent"})
                 }
                 if (input$showUnitCircle) {
                   lines(
                     1 * cc,
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
                   seq(rmin, 10, by = 1 / nc)
                 tmin <- pi / 12
                 for (t in (seq(tmin, 2 * pi, by = tmin))) {
                   r <- cos(t) * ell + (0 + (0 + 1i)) * sin(t) * ell
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
                                                                                   1)))
                       }
                       else if ((t > pi / 3) &&
                                (t < pi)) {
                         substitute(paste(frac(MYVALUE1 * pi, MYVALUE2)),
                                    list(
                                      MYVALUE1 = sub("^1$",
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
                       else if ((t >= pi) &&
                                (t <= 2 * pi)) {
                         substitute(paste(MYVALUE, pi), list(MYVALUE = round((t / pi),
                                                                             2)))
                       }
                     },
                     col = if (input$polargrid) {input$grcolor} else {"transparent"},
                     adj = c(0.5 - 1 * cos(t), 0.5 - 1 * sin(t))
                   )
                 }
                 tmin <- pi / 8
                 for (t in (seq(tmin, 2 * pi, by = tmin))) {
                   r <- cos(t) * ell + (0 + (0 + 1i)) * sin(t) * ell
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
                                                                                   1)))
                       }
                       else if ((t > pi / 3) &&
                                (t < pi)) {
                         substitute(paste(frac(MYVALUE1 * pi, MYVALUE2)),
                                    list(
                                      MYVALUE1 = sub("^1$",
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
                       else if ((t >= pi) &&
                                (t <= 2 * pi)) {
                         substitute(paste(MYVALUE, pi), list(MYVALUE = round((t / pi),
                                                                             2)))
                       }
                     },
                     col = if (input$polargrid) {input$grcolor} else {"transparent"},
                     adj = c(0.5 - 1.05 * cos(t), 0.5 - 1.05 * sin(t))
                   )
                 }
                 grid(col = if (input$polargrid) {input$grcolor} else {"transparent"})
                 abline(h = 0, col = "black")
                 abline(v = 0, col = "black")
                 points(
                   Re(handles$zeroloc),
                   Im(handles$zeroloc),
                   pch = 21,
                   cex = input$LineWidth,
                   lwd = input$LineWidth,
                   col = input$ForegroundColorZeros,
                   bg = input$BackgroundColor
                 )
                 points(
                   Re(handles$poleloc),
                   Im(handles$poleloc),
                   pch = 4,
                   cex = input$LineWidth,
                   lwd = input$LineWidth,
                   col = input$ForegroundColorPoles
                 )
                 nCentralBalancePoints <-
                   length(handles$poleloc) - length(handles$zeroloc)
                 if ((length(handles$poleloc) == 1) &&
                     (handles$poleloc[1] ==
                      0)) {
                   nCentralBalancePoints <- nCentralBalancePoints - 1
                 }
                 else if ((length(handles$zeroloc) == 1) &&
                          (handles$zeroloc[1] ==
                           0)) {
                   nCentralBalancePoints <- nCentralBalancePoints + 1
                 }
                 if (nCentralBalancePoints > 0) {
                   points(
                     rep(0, nCentralBalancePoints),
                     rep(0, nCentralBalancePoints),
                     pch = 21,
                     cex = input$LineWidth / 2,
                     col = input$ForegroundColorZeros,
                     bg = input$BackgroundColor,
                     lwd = input$LineWidth
                   )
                   if (nCentralBalancePoints > 1) {
                     text(
                       0,
                       0,
                       labels = nCentralBalancePoints,
                       pos = 1,
                       offset = 0.8,
                       cex = input$LineWidth /
                         2,
                       col = input$ForegroundColorZeros,
                       bg = input$BackgroundColor
                     )
                   }
                 }
                 else if (nCentralBalancePoints < 0) {
                   points(
                     rep(0,-nCentralBalancePoints),
                     rep(0,-nCentralBalancePoints),
                     pch = 4,
                     cex = input$LineWidth / 2,
                     col = input$ForegroundColorPoles,
                     lwd = input$LineWidth
                   )
                   if (-nCentralBalancePoints > 1) {
                     text(
                       0,
                       0,
                       labels = -nCentralBalancePoints,
                       pos = 3,
                       offset = 0.7,
                       cex = input$LineWidth / 2,
                       col = input$ForegroundColorPoles
                     )
                   }
                 }
                 MyTableZeros <-
                   matrix(data = 0,
                          nrow = length(handles$zeroloc),
                          ncol = 2)
                 MyTableZeros[, 1] <-
                   handles$zeroloc
                 for (i in 1:NROW(MyTableZeros)) {
                   MyTableZeros[i, 2] <- sum(MyTableZeros[, 1] == MyTableZeros[i,
                                                                               1])
                 }
                 MyTableZeros <-
                   MyTableZeros[(Re(MyTableZeros[, 2]) > 1),]
                 MyTableZeros <-
                   unique(MyTableZeros)
                 if (NROW(MyTableZeros) > 0) {
                   text(
                     Re(MyTableZeros[, 1]),
                     Im(MyTableZeros[, 1]),
                     labels = Re(MyTableZeros[,
                                              2]),
                     pos = 1,
                     offset = 0.8,
                     cex = input$LineWidth * 2 / 3,
                     col = input$ForegroundColorZeros,
                     bg = input$BackgroundColor
                   )
                 }
                 MyTablePoles <-
                   matrix(data = 0,
                          nrow = length(handles$poleloc),
                          ncol = 2)
                 isolate({
                   MyTablePoles[, 1] <- handles$poleloc
                   for (i in 1:NROW(MyTablePoles)) {
                     MyTablePoles[i, 2] <- sum(MyTablePoles[, 1] == MyTablePoles[i,
                                                                                 1])
                   }
                   MyTablePoles <-
                     MyTablePoles[(Re(MyTablePoles[, 2]) > 1),]
                   MyTablePoles <-
                     unique(MyTablePoles)
                   if (NROW(MyTablePoles) > 0) {
                     text(
                       Re(MyTablePoles[, 1]),
                       Im(MyTablePoles[, 1]),
                       labels = Re(MyTablePoles[,
                                                2]),
                       pos = 3,
                       offset = 0.7,
                       cex = input$LineWidth *
                         2 / 3,
                       col = input$ForegroundColorPoles
                     )
                   }
                 })
               })
  
  # output$axes_pzplotPolar ----
  output$axes_pzplotPolar <-
    renderPlot(width = "auto", height = "auto",
               {
                 nc <- 100
                 cc <-
                   1 * exp((0 + (0 + 1i)) * 2 * pi * c(0:(nc - 1)) / (nc - 1))
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
                   switch(3 + sign(sin(
                     input$slider1 * pi / (input$samplingfreq / 2)
                   )) +
                     sign(cos(
                       input$slider1 * pi / (input$samplingfreq / 2)
                     )),
                   {
                     rayX <- c(0,-10)
                     rayY <-
                       c(0,-10 * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   },
                   if (abs(sin(
                     input$slider1 * pi / (input$samplingfreq / 2)
                   )) <=
                   2 * eps) {
                     rayX <- c(0,-10)
                     rayY <- c(0, 0)
                   } else {
                     rayX <- c(0, 0)
                     rayY <-
                       c(0,-10)
                   },
                   if (sin(input$slider1 * pi / (input$samplingfreq / 2)) >
                       0) {
                     rayX <- c(0,-10)
                     rayY <-
                       c(0,-10 * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   } else {
                     rayX <- c(0, 10)
                     rayY <-
                       c(0, 10 * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   },
                   if (abs(sin(
                     input$slider1 * pi / (input$samplingfreq / 2)
                   )) <=
                   2 * eps) {
                     rayX <- c(0, 10)
                     rayY <- c(0, 0)
                   } else {
                     rayX <- c(0, 0)
                     rayY <-
                       c(0, 10)
                   },
                   {
                     rayX <- c(0, 10)
                     rayY <-
                       c(0, 10 * tan(input$slider1 * pi / (input$samplingfreq / 2)))
                   })
                   if (abs(rayX[1]) < eps)
                     rayX[1] <- 0
                   if (abs(rayX[2]) < eps)
                     rayX[2] <- 0
                   if (abs(rayY[1]) < eps)
                     rayY[1] <- 0
                   if (abs(rayY[2]) < eps)
                     rayY[2] <- 0
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
                   pch = 21,
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
                   pch = 4,
                   col = input$ForegroundColorPoles,
                   add = TRUE,
                   cex = input$LineWidth
                 )
                 axis(side = 1)
                 axis(side = 2)
                 if (input$showUnitCircle) {
                   lines(
                     sin(seq(
                       from = 0,
                       to = 2 * pi,
                       by = 2 * pi / 40
                     )),
                     cos(seq(
                       from = 0,
                       to = 2 * pi,
                       by = 2 * pi / 40
                     )),
                     col = input$ForegroundColor,
                     lty = "solid",
                     lwd = input$LineWidth
                   )
                 }
                 grid(col = if (input$polargrid) {input$grcolor} else {"transparent"})
                 abline(h = 0, col = "black")
                 abline(v = 0, col = "black")
                 pracma::polar(
                   atan2(Im(handles$zeroloc), Re(handles$zeroloc)),
                   sqrt(
                     Re(handles$zeroloc) * Re(handles$zeroloc) + Im(handles$zeroloc) *
                       Im(handles$zeroloc)
                   ),
                   type = "p",
                   lwd = input$LineWidth,
                   pch = 21,
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
                   pch = 4,
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
                   Re(filtb) + j * 1e+12
                 filtb[Im(filtb) < -1e+12] <-
                   Re(filtb) - j * 1e+12
                 filta[Im(filta) > 1e+12] <-
                   Re(filtb) + j * 1e+12
                 filta[Im(filta) < -1e+12] <-
                   Re(filtb) - j * 1e+12
                 rv <-
                   signal::freqz(
                     filtb,
                     filta,
                     region = "whole",
                     n = 2 ^ 20,
                     Fs = 2 * pi * input$samplingfreq /
                       2
                   )
                 radjusted <-
                   60 + 20 * log10(Mod(rv$h)) - 20 * log10(max(Mod(rv$h),
                                                               na.rm = TRUE))
                 radjusted[radjusted < 0] <-
                   0
                 radjusted[radjusted > 60] <-
                   60
                 pracma::polar(
                   t = rv$f,
                   r = radjusted,
                   xlim = c(0, 60),
                   ylim = c(0,
                            60),
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
                   side = 2,
                   pos = 0,
                   at = c(0, 10, 20, 30, 40, 50, 60),
                   labels = c(-60,-50,-40,-30,-20,-10, 0),
                   las = 1
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
                   z <- rv$X + rv$Y * j
                   shiny::setProgress(0.2)
                   aPolyValueAtz <- pracma::polyval(handlesa(), z)
                   if (max(abs(aPolyValueAtz)) > 2 * eps) {
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
                   Hz <- HzReal + HzImag * j
                   filtb <- handlesb()
                   filta <- handlesa()
                   filtb[Re(filtb) > 1e+12] <- 1e+12
                   filtb[Re(filtb) < -1e+12] <- -1e+12
                   filta[Re(filta) > 1e+12] <- 1e+12
                   filta[Re(filta) < -1e+12] <- -1e+12
                   filtb[Im(filtb) > 1e+12] <- Re(filtb) + j * 1e+12
                   filtb[Im(filtb) < -1e+12] <-
                     Re(filtb) - j * 1e+12
                   filta[Im(filta) > 1e+12] <- Re(filtb) + j * 1e+12
                   filta[Im(filta) < -1e+12] <-
                     Re(filtb) - j * 1e+12
                   rv <-
                     signal::freqz(filtb, filta, region = "whole", n = 1024)
                   xx <- rep(x, times = length(y))
                   yy <- rep(y, each = length(x))
                   zz <- as.vector(Mod(Hz))
                   dim(zz) <- NULL
                   
                   # http://blog.revolutionanalytics.com/2016/02/multivariate_data_with_r.html
                   ra <-
                     ceiling(input$colors3D * (20 * log10(zz) - 20 * log10(min(
                       Mod(Hz),
                       na.last = TRUE
                     ))) / (20 * log10(max(zz)) - 20 * log10(min(zz))))
                   col <- topo.colors(input$colors3D)
                   shiny::setProgress(0.4)
                   zz <- 20 * log10(zz)
                   zz[zz == 0] <- NA
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
                         1 * cos(rv$f + pi / 2),
                         1 * sin(rv$f +
                                   pi /
                                   2),
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
                       seq(-2.5, 2.5, length.out = 1001),
                       0,
                       0,
                       color = "black",
                       labels = "x-axis",
                       size = 0.1
                     )
                     x$points3d(
                       0,
                       seq(-2.5, 2.5, length.out = 1001),
                       0,
                       color = "black",
                       labels = "y-axis",
                       size = 0.1
                     )
                     x$points3d(
                       0,
                       0,
                       seq(-2.5, 2.5, length.out = 1001),
                       color = "black",
                       labels = "z-axis",
                       size = 0.1
                     )
                     x$points3d(
                       seq(-2.5, 2.5, length.out = 20),
                       seq(-2.5,
                           2.5, length.out = 20),
                       0,
                       color = "black",
                       labels = "grid z=0",
                       size = 0.2
                     )
                     x$points3d(
                       seq(-2.5, 2.5, length.out = input$nticks3D),
                       seq(-2.5, 2.5, length.out = 201),
                       0,
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
                   x <- seq(-1.3, 1.3, length.out = 256)
                   y <- seq(-1.3, 1.3, length.out = 256)
                   rvmg <- pracma::meshgrid(x, y)
                   z <- t(rvmg$X + rvmg$Y * j)
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
                   HdB <- HdBReal + HdBImag * j
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
                       Re(filtb) + j * 1e+12
                     filtb[Im(filtb) < -1e+12] <-
                       Re(filtb) - j * 1e+12
                     filta[Im(filta) > 1e+12] <-
                       Re(filtb) + j * 1e+12
                     filta[Im(filta) < -1e+12] <-
                       Re(filtb) - j * 1e+12
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
                     rvh <- rvhReal + rvhImag * j
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
                         -130, min(20 * log10(Mod(
                           input$edit_gain *
                             rvh
                         )), na.rm = TRUE) - 10
                       ), na.rm = TRUE), min(c(
                         130,
                         max(20 * log10(Mod(
                           input$edit_gain * rvh
                         )), na.rm = TRUE) +
                           10
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
                       1 * cos(rv$f),
                       1 * sin(rv$f),
                       20 * log10(Mod(input$edit_gain * rvh)),
                       type = "h",
                       col = "red",
                       lwd = input$LineWidth,
                       lty = "dashed",
                       add = TRUE,
                       zlim = c(max(c(
                         -130, min(20 *
                                     log10(Mod(
                                       input$edit_gain * rvh
                                     )), na.rm = TRUE) -
                           10
                       ), na.rm = TRUE), min(c(
                         130, max(20 * log10(Mod(
                           input$edit_gain * rvh
                         )), na.rm = TRUE) + 10
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                     rgl::lines3d(
                       1 * cos(rv$f),
                       1 * sin(rv$f),
                       20 * log10(Mod(input$edit_gain * rvh)),
                       col = "red",
                       lwd = 10 * input$LineWidth,
                       add = TRUE,
                       zlim = c(max(c(
                         -130, min(20 * log10(Mod(
                           input$edit_gain * rvh
                         )), na.rm = TRUE) - 10
                       ), na.rm = TRUE), 
                       min(c(
                         130,
                         max(20 * log10(Mod(
                           input$edit_gain * rvh
                         )), na.rm = TRUE) +
                           10
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                   }
                   if (input$checkboxRAY) {
                     switch(3 + sign(sin(
                       input$slider1 * pi / (input$samplingfreq / 2)
                     )) +
                       sign(cos(
                         input$slider1 * pi / (input$samplingfreq / 2)
                       )),
                     {
                       rayX <- c(0, 1.04 * cos(input$slider1 * pi / (input$samplingfreq / 2)))
                       rayY <-
                         c(0, 1.04 * sin(input$slider1 * pi / (input$samplingfreq / 2)))
                     },
                     if (abs(sin(
                       input$slider1 * pi / (input$samplingfreq / 2)
                     )) <=
                     2 * eps) {
                       rayX <- c(0, 1.04 * cos(input$slider1 * pi / (input$samplingfreq / 2)))
                       rayY <- c(0, 0)
                     } else {
                       rayX <- c(0, 0)
                       rayY <-
                         c(0, 1.04 * sin(input$slider1 * pi / (input$samplingfreq / 2)))
                     },
                     if (sin(input$slider1 * pi / (input$samplingfreq / 2)) > 0) {
                       rayX <- c(0, 1.04 * cos(input$slider1 * pi / (input$samplingfreq / 2)))
                       rayY <-
                         c(0, 1.04 * sin(input$slider1 * pi / (input$samplingfreq / 2)))
                     } else {
                       rayX <- c(0, 1.04 * cos(input$slider1 * pi / (input$samplingfreq / 2)))
                       rayY <-
                         c(0, 1.04 * sin(input$slider1 * pi / (input$samplingfreq / 2)))
                     },
                     if (abs(sin(
                       input$slider1 * pi / (input$samplingfreq / 2) )) <= 2 * eps
                     ) {
                       rayX <- c(0, 1.04 * cos(input$slider1 * pi / (input$samplingfreq / 2)))
                       rayY <- c(0, 0)
                     } else {
                       rayX <- c(0, 0)
                       rayY <-
                         c(0, 1.04 * sin(input$slider1 * pi / (input$samplingfreq / 2)))
                     },
                     {
                       rayX <- c(0, 1.04 * cos(input$slider1 * pi / (input$samplingfreq / 2)))
                       rayY <-
                         c(0, 1.04 * sin(input$slider1 * pi / (input$samplingfreq / 2)))
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
                       c(0, max(20 * log10(
                         Mod(input$edit_gain * rvh)
                       ), na.rm = TRUE)),
                       lwd = 10 * input$LineWidth,
                       col = "magenta",
                       add = TRUE,
                       zlim = c(max(c(
                         -130, min(20 *
                                     log10(Mod(
                                       input$edit_gain * rvh
                                     )), na.rm = TRUE) - 10
                       ), na.rm = TRUE), min(c(
                         130, max(20 * log10(Mod(
                           input$edit_gain *
                             rvh
                         )), na.rm = TRUE) + 10
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                     rgl::lines3d(
                       rayX,
                       rayY,
                       c(0, max(20 * log10(
                         Mod(input$edit_gain *
                               rvh)
                       ), na.rm = TRUE)),
                       lwd = 10 * input$LineWidth,
                       col = "magenta",
                       add = TRUE,
                       zlim = c(max(c(
                         -130, min(20 *
                                     log10(Mod(
                                       input$edit_gain * rvh
                                     )), na.rm = TRUE) -
                           10
                       ), na.rm = TRUE), min(c(
                         130, max(20 * log10(Mod(
                           input$edit_gain *
                             rvh
                         )), na.rm = TRUE) + 10
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                     rgl::plot3d(
                       rayX,
                       rayY,
                       c(0, min(20 * log10(
                         Mod(input$edit_gain *
                               rvh)
                       ))),
                       type = "h",
                       lwd = input$LineWidth,
                       col = "magenta",
                       add = TRUE,
                       zlim = c(max(c(
                         -130, min(20 * log10(Mod(
                           input$edit_gain *
                             rvh
                         )), na.rm = TRUE) - 10
                       ), na.rm = TRUE), min(c(
                         130,
                         max(20 * log10(Mod(
                           input$edit_gain * rvh
                         )), na.rm = TRUE) +
                           10
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                     rgl::plot3d(
                       rayX,
                       rayY,
                       c(0, max(20 * log10(
                         Mod(input$edit_gain *
                               rvh)
                       ), na.rm = TRUE)),
                       type = "s",
                       size = 1,
                       lwd = input$LineWidth,
                       col = "magenta",
                       add = TRUE,
                       zlim = c(max(c(
                         -130, min(20 *
                                     log10(Mod(
                                       input$edit_gain * rvh
                                     )), na.rm = TRUE) -
                           10
                       ), na.rm = TRUE), min(c(
                         130, max(20 * log10(Mod(
                           input$edit_gain *
                             rvh
                         )), na.rm = TRUE) + 10
                       ), na.rm = TRUE)),
                       forceClipregion = TRUE
                     )
                     rgl::arrow3d(c(rayX[1], rayY[1], min(c(
                       120, max(20 *
                                  log10(Mod(
                                    input$edit_gain * rvh
                                  )), na.rm = TRUE)
                     ),
                     na.rm = TRUE)),
                     c(rayX[2], rayY[2], min(c(
                       120, max(20 *
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
                   z <- t(rvmg$X + rvmg$Y * j)
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
                   HdB <- HdBReal + HdBImag * j
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
                       Re(filtb) + j * 1e+12
                     filtb[Im(filtb) < -1e+12] <-
                       Re(filtb) - j * 1e+12
                     filta[Im(filta) > 1e+12] <-
                       Re(filtb) + j * 1e+12
                     filta[Im(filta) < -1e+12] <-
                       Re(filtb) - j * 1e+12
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
                     rvh <- rvhReal + rvhImag * j
                   }

        # x <- seq(-1.3, 1.3, length.out = 256)
        # y <- seq(-1.3, 1.3, length.out = 256)
        # rv <- pracma::meshgrid(x, y)
        # z <- rv$X + rv$Y * j
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
        # HdB <- HdBReal + HdBImag * j
        # HdB <- 20 * log10(Mod(input$edit_gain * HdB))
        # filtb <- handlesb()
        # filta <- handlesa()
        # filtb[Re(filtb) > 1e+12] <- 1e+12
        # filtb[Re(filtb) < -1e+12] <- -1e+12
        # filta[Re(filta) > 1e+12] <- 1e+12
        # filta[Re(filta) < -1e+12] <- -1e+12
        # filtb[Im(filtb) > 1e+12] <- Re(filtb) + j * 1e+12
        # filtb[Im(filtb) < -1e+12] <- Re(filtb) - j * 1e+12
        # filta[Im(filta) > 1e+12] <- Re(filtb) + j * 1e+12
        # filta[Im(filta) < -1e+12] <- Re(filtb) - j * 1e+12
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
            -130, min(20 * log10(Mod(
              input$edit_gain * rvh
            )))
          ), na.rm = TRUE), min(c(
            130, max(20 * log10(Mod(
              input$edit_gain * rvh
            )), na.rm = TRUE)
          ), na.rm = TRUE))
        )
        if (input$showUnitCircle) {
          rgl::plot3d(
            1 * cos(rv$f + pi / 2),
            1 * sin(rv$f + pi / 2),
            20 * log10(Mod(input$edit_gain * rv$h)),
            type = "h",
            col = "red",
            lwd = input$LineWidth,
            lty = "dashed",
            add = TRUE,
            zlim = c(max(c(
              -130,
              min(20 * log10(Mod(
                input$edit_gain * rvh
              )), na.rm = TRUE) - 10
            ), na.rm = TRUE), min(c(
              130, max(20 * log10(Mod(
                input$edit_gain *
                  rvh
              )), na.rm = TRUE) + 10
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
    filtb[Im(filtb) > 1e+12] <- Re(filtb) + j * 1e+12
    filtb[Im(filtb) < -1e+12] <- Re(filtb) - j * 1e+12
    filta[Im(filta) > 1e+12] <- Re(filtb) + j * 1e+12
    filta[Im(filta) < -1e+12] <- Re(filtb) - j * 1e+12
    rv <-
      signal::freqz(
        filtb,
        filta,
        region = "whole",
        n = 2 ^ 20,
        Fs = 2 *
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
          20 * log10(1 / input$edit_gain)
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
          1 / input$edit_gain
        })
      },
      log = if (input$logarithmicFreqAxis) {
        "x"
      }
      else {
        ""
      },
      xlim = if (input$logarithmicFreqAxis) {
        c(max(0.1, 1e-04 * input$samplingfreq / 2),
          input$samplingfreq / 2)
      }
      else {
        c(if (input$twosidedFFT) {
          -input$samplingfreq / 2
        } else {
          0
        }, if (input$FFTshifted) {
          input$samplingfreq / 2
        } else {
          if (input$twosidedFFT) {
            input$samplingfreq / 2
          } else {
            2 * input$samplingfreq / 2
          }
        })
      },
      ylim = if (input$logarithmicMagPlotAmplitude) {
        c(max(-130, min(-20, 20 * log10(
          min(Mod(rv$h), na.rm = TRUE) / (if (input$normalizedMagPlotAmplitude) {
            max(Mod(rv$h), na.rm = TRUE)
          } else {
            1 / input$edit_gain
          })
        ))), max(0, min(120, 20 * log10(
          max(Mod(rv$h), na.rm = TRUE) / (if (input$normalizedMagPlotAmplitude) {
            max(Mod(rv$h), na.rm = TRUE)
          } else {
            1 / input$edit_gain
          })
        ), na.rm = TRUE)))
      }
      else {
        c(0, max(1, Mod(rv$h) / (if (input$normalizedMagPlotAmplitude) {
          max(Mod(rv$h), na.rm = TRUE)
        } else {
          1 / input$edit_gain
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
        expression(2 * omega ~ ~  ~  ~ "[rads]")
      }
      else if (input$freqaxisunits == "zero2fs") {
        if (input$twosidedFFT)
          expression(-f[s] %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
        else
          expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
      }
      else if (input$freqaxisunits == "zero2fsby2") {
        if ((!input$twosidedFFT) &&
            (!input$FFTshifted) && (!input$logarithmicFreqAxis)) {
          expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
        }
        else {
          expression(0 %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
        }
      }
      else if (input$freqaxisunits == "zero2fmax") {
        expression(0 %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
      }
      else if (input$freqaxisunits == "zero2fmaxby2") {
        if (input$twosidedFFT)
          expression(-f[max] / 2 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
        else
          expression(0 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
      },
      ylab = if ((input$freqaxisunits == "zero2one") ||
                 (input$freqaxisunits ==
                  "zero2pi") ||
                 (input$freqaxisunits == "zero22pi") || (input$freqaxisunits ==
                                                         "zero2half") ||
                 (input$freqaxisunits == "zero2piby2")) {
        if (input$normalizedMagPlotAmplitude) {
          if (input$logarithmicMagPlotAmplitude) {
            expression(paste(group("|", bold(H(
              e ^ {
                j * omega
              }
            )), "|"), " / max [dB]"))
          }
          else {
            expression(paste(group("|", bold(H(
              e ^ {
                j * omega
              }
            )), "|")), " / max")
          }
        }
        else {
          if (input$logarithmicMagPlotAmplitude) {
            expression(paste(group("|", bold(H(
              e ^ {
                j * omega
              }
            )), "|"), "  [dB]"))
          }
          else {
            expression(group("|", bold(H(e ^ {
              j * omega
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
    abline(h = 0, col = "black")
    abline(v = 0, col = "black")
    if (input$secondaryaxis) {
      if ((input$freqaxisunits == "zero2pi") || (input$freqaxisunits ==
                                                 "zero22pi") ||
          (input$freqaxisunits == "zero2piby2")) {
        axis(
          side = 3,
          at = pi * c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
          labels = expression(-6*pi,-11*pi/2,            -5 * pi,            -9 * pi / 2,            -4 * pi,            -7 * pi / 2,-3 * pi,            -5 * pi / 2,            -2 * pi,            -3 * pi / 2,            -pi,            -pi / 2,            -pi / 3,-pi / 5,            pi / 5,            pi / 3,            pi / 2,            pi,            3 * pi / 2,            2 * pi,            5 * pi / 2,            3 * pi,            7 * pi / 2,            4 * pi,            9 * pi / 2,            5 * pi,            11 * pi / 2,            6 * pi          ),
          tick = TRUE,
          pos = par("usr")[4],
          outer = FALSE,
          col = input$grcolor,
          cex.axis = 0.8,
          mgp = c(-2.5,-1,
                  0),
          xlab = "pi-based"
        )
      }
      if ((input$freqaxisunits == "zero2one") ||
          (input$freqaxisunits ==
           "zero2half")) {
        axis(
          side = 3,
          at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
          labels = expression(-6*pi,-11*pi/2,            -5 * pi,            -9 * pi / 2,            -4 * pi,            -7 * pi / 2,-3 * pi,            -5 * pi / 2,            -2 * pi,            -3 * pi / 2,            -pi,            -pi / 2,            -pi / 3,-pi / 5,            pi / 5,            pi / 3,            pi / 2,            pi,            3 * pi / 2,            2 * pi,            5 * pi / 2,            3 * pi,            7 * pi / 2,            4 * pi,            9 * pi / 2,            5 * pi,            11 * pi / 2,            6 * pi          ),
          tick = TRUE,
          pos = par("usr")[4],
          outer = FALSE,
          col = input$grcolor,
          cex.axis = 0.8,
          mgp = c(-2.5,-1,
                  0)
        )
      }
      if ((input$freqaxisunits == "zero2fs") ||
          (input$freqaxisunits ==
           "zero2fmax") ||
          (input$freqaxisunits == "zero2fmaxby2")) {
        axis(
          side = 3,
          at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6
          ) * input$samplingfreq / 2,
          labels = expression(-6*pi,-11*pi/2,            -5 * pi,            -9 *              pi / 2,            -4 * pi,            -7 * pi / 2,            -3 * pi,            -5 * pi / 2,            -2 *              pi,            -3 * pi / 2,            -pi,            -pi / 2,            -pi / 3,            -pi / 5,            pi / 5,            pi / 3,            pi / 2,            pi,            3 * pi / 2,            2 * pi,            5 * pi / 2,            3 * pi,            7 * pi / 2,            4 * pi,            9 * pi / 2,            5 * pi,            11 * pi / 2,            6 * pi          ),
          tick = TRUE,
          line = -0.4,
          pos = par("usr")[4],
          outer = FALSE,
          col = input$grcolor,
          cex.axis = 0.8,
          mgp = c(-2.5,-1, 0)
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
        lwd = input$LineWidth * 2 / 3,
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
          minpeakdistance = (length(rv$h) / 100) / 2,
          threshold = 0.001,
          npeaks = 100
        )
      xmins <-
        myfindpeaks(
          -Mod(rv$h),
          minpeakdistance = (length(rv$h) / 100) / 2,
          threshold = 0.001,
          npeaks = 100
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
              indxs <- which(isolate(handles$magnmaximumsa) < (20 *
                                                                 log10(max(
                                                                   Mod(rv$h), na.rm = TRUE
                                                                 )) - 0.01))
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
                20 * log10(1 / input$edit_gain)
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
              indxs <- which(isolate(handles$magnmaximumsa) < (20 *
                                                                 log10(max(
                                                                   Mod(rv$h), na.rm = TRUE
                                                                 )) - 0.01))
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
                20 * log10(1 / input$edit_gain)
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
          lwd = input$LineWidth * 2 / 3,
          pch = 24,
          cex = 2,
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
          pch = 25,
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
                   Re(filtb) + j * 1e+12
                 filtb[Im(filtb) < -1e+12] <-
                   Re(filtb) - j * 1e+12
                 filta[Im(filta) > 1e+12] <-
                   Re(filtb) + j * 1e+12
                 filta[Im(filta) < -1e+12] <-
                   Re(filtb) - j * 1e+12
                 rv <-
                   signal::freqz(
                     filtb,
                     filta,
                     region = "whole",
                     n = 2 ^ 20,
                     Fs = 2 * pi * input$samplingfreq /
                       2
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
                       20 * log10(1 / input$edit_gain)
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
                       1 / input$edit_gain
                     })
                   },
                   xlim = input$zoomlimXpassband,
                   ylim = input$zoomlimYpassband,
                   type = "l",
                   col = input$ForegroundColor,
                   bg = input$BackgroundColor,
                   lwd = input$LineWidth,
                   xlab = if (input$freqaxisunits ==
                              "zero2one") {
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
                     expression(2 * omega ~ ~  ~  ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero2fs") {
                     if (input$twosidedFFT)
                       expression(-f[s] %->% ~
                                    ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                     else
                       expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                   }
                   else if (input$freqaxisunits == "zero2fsby2") {
                     if ((!input$twosidedFFT) &&
                         (!input$FFTshifted) && (!input$logarithmicFreqAxis)) {
                       expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                     }
                     else {
                       expression(0 %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
                     }
                   }
                   else if (input$freqaxisunits == "zero2fmax") {
                     expression(0 %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
                   }
                   else if (input$freqaxisunits == "zero2fmaxby2") {
                     if (input$twosidedFFT)
                       expression(-f[max] /
                                    2 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
                     else
                       expression(0 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
                   },
                   ylab = if ((input$freqaxisunits == "zero2one") ||
                              (input$freqaxisunits ==
                               "zero2pi") ||
                              (input$freqaxisunits == "zero22pi") ||
                              (input$freqaxisunits == "zero2half") ||
                              (input$freqaxisunits ==
                               "zero2piby2")) {
                     if (input$normalizedMagPlotAmplitude) {
                       if (input$logarithmicMagPlotAmplitude) {
                         expression(paste(group("|", bold(H(
                           e ^ {
                             j * omega
                           }
                         )), "|"), " / max [dB]"))
                       }
                       else {
                         expression(paste(group("|", bold(H(
                           e ^ {
                             j * omega
                           }
                         )), "|")), " / max")
                       }
                     }
                     else {
                       if (input$logarithmicMagPlotAmplitude) {
                         expression(paste(group("|", bold(H(
                           e ^ {
                             j * omega
                           }
                         )), "|"), "  [dB]"))
                       }
                       else {
                         expression(group("|", bold(H(e ^ {
                           j * omega
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
                 abline(h = 0, col = "black")
                 abline(v = 0, col = "black")
                 if (input$secondaryaxis) {
                   if ((input$freqaxisunits == "zero2pi") || (input$freqaxisunits ==
                                                              "zero22pi") ||
                       (input$freqaxisunits == "zero2piby2")) {
                     axis(
                       side = 3,
                       at = pi * c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
                       labels = expression(-6*pi,-11*pi/2,                         -5 * pi,                         -9 * pi / 2,                         -4 * pi,                         -7 * pi / 2,-3 * pi,                         -5 * pi / 2,                         -2 * pi,                         -3 * pi / 2,                         -pi,                         -pi / 2,-pi / 3,                         -pi / 5,                         pi / 5,                         pi / 3,                         pi / 2,                         pi,                         3 * pi / 2,                         2 * pi,                         5 * pi /                           2,                         3 * pi,                         7 * pi / 2,                         4 * pi,                         9 * pi / 2,                         5 * pi,                         11 * pi /                           2,                         6 * pi                       ),
                       tick = TRUE,
                       pos = par("usr")[4],
                       outer = FALSE,
                       col = input$grcolor,
                       cex.axis = 0.8,
                       mgp = c(-2.5,-1, 0),
                       xlab = "pi-based"
                     )
                   }
                   if ((input$freqaxisunits == "zero2one") ||
                       (input$freqaxisunits ==
                        "zero2half")) {
                     axis(
                       side = 3,
                       at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
                       labels = expression(-6*pi,-11*pi/2,                         -5 * pi,                         -9 * pi / 2,                         -4 * pi,                         -7 * pi / 2,-3 * pi,                         -5 * pi / 2,                         -2 * pi,                         -3 * pi / 2,                         -pi,                         -pi / 2,-pi / 3,                         -pi / 5,                         pi / 5,                         pi / 3,                         pi / 2,                         pi,                         3 * pi / 2,                         2 * pi,                         5 * pi /                           2,                         3 * pi,                         7 * pi / 2,                         4 * pi,                         9 * pi / 2,                         5 * pi,                         11 * pi /                           2,                         6 * pi                       ),
                       tick = TRUE,
                       pos = par("usr")[4],
                       outer = FALSE,
                       col = input$grcolor,
                       cex.axis = 0.8,
                       mgp = c(-2.5,-1, 0)
                     )
                   }
                   if ((input$freqaxisunits == "zero2fs") ||
                       (input$freqaxisunits ==
                        "zero2fmax") ||
                       (input$freqaxisunits == "zero2fmaxby2")) {
                     axis(
                       side = 3,
                       at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6
                       ) * input$samplingfreq / 2,
                       labels = expression(-6*pi,-11*pi/2,                         -5 * pi,                         -9 *                           pi /                           2,                         -4 * pi,                         -7 * pi / 2,                         -3 * pi,                         -5 * pi / 2,                         -2 *                           pi,                         -3 * pi / 2,                         -pi,                         -pi / 2,                         -pi / 3,                         -pi / 5,                         pi / 5,                         pi / 3,                         pi /                           2,                         pi,                         3 * pi / 2,                         2 * pi,                         5 * pi / 2,                         3 * pi,                         7 *                           pi /                           2,                         4 * pi,                         9 * pi / 2,                         5 * pi,                         11 * pi / 2,                         6 *                           pi                       ),
                       tick = TRUE,
                       line = -0.4,
                       pos = par("usr")[4],
                       outer = FALSE,
                       col = input$grcolor,
                       cex.axis = 0.8,
                       mgp = c(-2.5,-1, 0)
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
                     lwd = input$LineWidth * 2 / 3,
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
                       0
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
                       1 / input$edit_gain
                     })
                   },
                   xlim = input$zoomlimXstopband,
                   ylim = input$zoomlimYstopband,
                   type = "l",
                   col = input$ForegroundColor,
                   bg = input$BackgroundColor,
                   lwd = input$LineWidth,
                   xlab = if (input$freqaxisunits ==
                              "zero2one") {
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
                     expression(2 * omega ~ ~  ~  ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero2fs") {
                     if (input$twosidedFFT)
                       expression(-f[s] %->% ~
                                    ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                     else
                       expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                   }
                   else if (input$freqaxisunits == "zero2fsby2") {
                     if ((!input$twosidedFFT) &&
                         (!input$FFTshifted) && (!input$logarithmicFreqAxis)) {
                       expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                     }
                     else {
                       expression(0 %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
                     }
                   }
                   else if (input$freqaxisunits == "zero2fmax") {
                     expression(0 %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
                   }
                   else if (input$freqaxisunits == "zero2fmaxby2") {
                     if (input$twosidedFFT)
                       expression(-f[max] /
                                    2 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
                     else
                       expression(0 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
                   },
                   ylab = if ((input$freqaxisunits == "zero2one") ||
                              (input$freqaxisunits ==
                               "zero2pi") ||
                              (input$freqaxisunits == "zero22pi") ||
                              (input$freqaxisunits == "zero2half") ||
                              (input$freqaxisunits ==
                               "zero2piby2")) {
                     if (input$normalizedMagPlotAmplitude) {
                       if (input$logarithmicMagPlotAmplitude) {
                         expression(paste(group("|", bold(H(
                           e ^ {
                             j * omega
                           }
                         )), "|"), " / max [dB]"))
                       }
                       else {
                         expression(paste(group("|", bold(H(
                           e ^ {
                             j * omega
                           }
                         )), "|")), " / max")
                       }
                     }
                     else {
                       if (input$logarithmicMagPlotAmplitude) {
                         expression(paste(group("|", bold(H(
                           e ^ {
                             j * omega
                           }
                         )), "|"), "  [dB]"))
                       }
                       else {
                         expression(group("|", bold(H(e ^ {
                           j * omega
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
                 abline(h = 0, col = "black")
                 abline(v = 0, col = "black")
                 if (input$secondaryaxis) {
                   if ((input$freqaxisunits == "zero2pi") || (input$freqaxisunits ==
                                                              "zero22pi") ||
                       (input$freqaxisunits == "zero2piby2")) {
                     axis(
                       side = 3,
                       at = pi * c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
                       labels = expression(-6*pi,-11*pi/2,                         -5 * pi,                         -9 * pi / 2,                         -4 * pi,                         -7 * pi / 2,-3 * pi,                         -5 * pi / 2,                         -2 * pi,                         -3 * pi / 2,                         -pi,                         -pi / 2,-pi / 3,                         -pi / 5,                         pi / 5,                         pi / 3,                         pi / 2,                         pi,                         3 * pi / 2,                         2 * pi,                         5 * pi /                           2,                         3 * pi,                         7 * pi / 2,                         4 * pi,                         9 * pi / 2,                         5 * pi,                         11 * pi /                           2,                         6 * pi                       ),
                       tick = TRUE,
                       pos = par("usr")[4],
                       outer = FALSE,
                       col = input$grcolor,
                       cex.axis = 0.8,
                       mgp = c(-2.5,-1, 0),
                       xlab = "pi-based"
                     )
                   }
                   if ((input$freqaxisunits == "zero2one") ||
                       (input$freqaxisunits ==
                        "zero2half")) {
                     axis(
                       side = 3,
                       at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
                       labels = expression(-6*pi,-11*pi/2,                         -5 * pi,                         -9 * pi / 2,                         -4 * pi,                         -7 * pi / 2,-3 * pi,                         -5 * pi / 2,                         -2 * pi,                         -3 * pi / 2,                         -pi,                         -pi / 2,-pi / 3,                         -pi / 5,                         pi / 5,                         pi / 3,                         pi / 2,                         pi,                         3 * pi / 2,                         2 * pi,                         5 * pi /                           2,                         3 * pi,                         7 * pi / 2,                         4 * pi,                         9 * pi / 2,                         5 * pi,                         11 * pi /                           2,                         6 * pi                       ),
                       tick = TRUE,
                       pos = par("usr")[4],
                       outer = FALSE,
                       col = input$grcolor,
                       cex.axis = 0.8,
                       mgp = c(-2.5,-1, 0)
                     )
                   }
                   if ((input$freqaxisunits == "zero2fs") ||
                       (input$freqaxisunits ==
                        "zero2fmax") ||
                       (input$freqaxisunits == "zero2fmaxby2")) {
                     axis(
                       side = 3,
                       at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6
                       ) * input$samplingfreq / 2,
                       labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
                       tick = TRUE,
                       line = -0.4,
                       pos = par("usr")[4],
                       outer = FALSE,
                       col = input$grcolor,
                       cex.axis = 0.8,
                       mgp = c(-2.5,-1, 0)
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
                     lwd = input$LineWidth * 2 / 3,
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
    filtb[Im(filtb) > 1e+12] <- Re(filtb) + j * 1e+12
    filtb[Im(filtb) < -1e+12] <- Re(filtb) - j * 1e+12
    filta[Im(filta) > 1e+12] <- Re(filtb) + j * 1e+12
    filta[Im(filta) < -1e+12] <- Re(filtb) - j * 1e+12
    rv <-
      signal::freqz(
        filtb,
        filta,
        region = "whole",
        n = 2 ^ 20,
        Fs = 2 *
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
        0
      }, if (input$FFTshifted) {
        input$samplingfreq / 2
      } else {
        if (input$twosidedFFT) {
          input$samplingfreq / 2
        } else {
          2 * input$samplingfreq / 2
        }
      }),
      ylim = if (input$unwrapPhase) {
        NULL
      }
      else if (input$degreesgrid) {
        NULL
      }
      else {
        c(-1, 1)
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
        expression(2 * omega ~ ~  ~  ~ "[rads]")
      }
      else if (input$freqaxisunits == "zero2fs") {
        if (input$twosidedFFT)
          expression(-f[s] %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
        else
          expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
      }
      else if (input$freqaxisunits == "zero2fsby2") {
        if ((!input$twosidedFFT) &&
            (!input$FFTshifted) && (!input$logarithmicFreqAxis)) {
          expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
        }
        else {
          expression(0 %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
        }
      }
      else if (input$freqaxisunits == "zero2fmax") {
        expression(0 %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
      }
      else if (input$freqaxisunits == "zero2fmaxby2") {
        if (input$twosidedFFT)
          expression(-f[max] / 2 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
        else
          expression(0 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
      },
      ylab = if ((input$freqaxisunits == "zero2one") ||
                 (input$freqaxisunits ==
                  "zero2pi") ||
                 (input$freqaxisunits == "zero22pi") || (input$freqaxisunits ==
                                                         "zero2half") ||
                 (input$freqaxisunits == "zero2piby2")) {
        if (input$degreesgrid) {
          expression(paste("Angle ", bold(H(e ^ {
            j * omega
          }))))
        }
        else {
          expression(paste("Angle ", bold(H(e ^ {
            j * omega
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
    abline(h = 0, col = "black")
    abline(v = 0, col = "black")
    if (input$secondaryaxis) {
      axis(
        side = 4,
        at = if (input$degreesgrid) {
          180 * c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6)
        }
        else {
          c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6)
        },
        labels = expression(
          -6 * pi,
          -frac(11 * pi, 2),
          -5 * pi,
          -frac(9 * pi, 2),
          -4 * pi,
          -frac(7 * pi, 2),
          -3 * pi,
          -frac(5 * pi, 2),
          -2 * pi,
          -frac(3 * pi, 2),
          -pi,
          frac(-pi, 2),
          frac(-pi, 3),
          frac(-pi, 5),
          frac(pi, 5),
          frac(pi, 3),
          frac(pi, 2),
          pi,
          frac(3 * pi, 2),
          2 * pi,
          frac(5 * pi, 2),
          3 * pi,
          frac(7 * pi, 2),
          4 * pi,
          frac(9 * pi, 2),
          5 * pi,
          frac(11 * pi, 2),
          6 * pi
        ),
        tick = TRUE,
        pos = par("usr")[2],
        outer = FALSE,
        col = input$grcolor,
        cex.axis = 0.8,
        las = 1
      )
      if ((input$freqaxisunits == "zero2pi") ||
          (input$freqaxisunits ==
           "zero22pi") ||
          (input$freqaxisunits == "zero2piby2")) {
        axis(
          side = 3,
          at = pi * c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
          labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
          tick = TRUE,
          pos = par("usr")[4],
          outer = FALSE,
          col = input$grcolor,
          cex.axis = 0.8,
          mgp = c(-2.5,-1,0)
        )
      }
      if ((input$freqaxisunits == "zero2one") ||
          (input$freqaxisunits ==
           "zero2half")) {
        axis(
          side = 3,
          at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6),
          labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
          tick = TRUE,
          pos = par("usr")[4],
          outer = FALSE,
          col = input$grcolor,
          cex.axis = 0.8,
          mgp = c(-2.5,-1,0)
        )
      }
      if ((input$freqaxisunits == "zero2fs") ||
          (input$freqaxisunits ==
           "zero2fmax") ||
          (input$freqaxisunits == "zero2fmaxby2")) {
        axis(
          side = 3,
          at = c(-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-1/2,-1/3,-1/5,1/5,1/3,1/2,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6
          ) * input$samplingfreq / 2,
          labels=expression(-6*pi,-11*pi/2,-5*pi,-9*pi/2,-4*pi,-7*pi/2,-3*pi,-5*pi/2,-2*pi,-3*pi/2,-pi,-pi/2,-pi/3,-pi/5,pi/5,pi/3,pi/2,pi,3*pi/2,2*pi,5*pi/2,3*pi,7*pi/2,4*pi,9*pi/2,5*pi,11*pi/2,6*pi),
          tick = TRUE,
          line = -0.4,
          pos = par("usr")[4],
          outer = FALSE,
          col = input$grcolor,
          cex.axis = 0.8,
          mgp = c(-2.5,-1, 0)
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
                     0
                   }, if (input$FFTshifted) {
                     input$samplingfreq / 2
                   } else {
                     if (input$twosidedFFT) {
                       input$samplingfreq / 2
                     } else {
                       2 * input$samplingfreq / 2
                     }
                   }),
                   ylim = c(max(c(
                     -10, min(rv$gd, max(rv$gd, na.rm = TRUE) -
                                1, na.rm = TRUE)
                   ), na.rm = TRUE), min(c(
                     100, max(rv$gd,
                              min(rv$gd, na.rm = TRUE) + 1, na.rm = TRUE)
                   ), na.rm = TRUE)),
                   type = "l",
                   col = input$ForegroundColor,
                   bg = input$BackgroundColor,
                   lwd = input$LineWidth,
                   xlab = if (input$freqaxisunits ==
                              "zero2one") {
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
                     expression(2 * omega ~ ~  ~  ~ "[rads]")
                   }
                   else if (input$freqaxisunits == "zero2fs") {
                     if (input$twosidedFFT)
                       expression(-f[s] %->% ~  ~  ~
                                    ~ f[s] ~ ~  ~  ~ "[Hz]")
                     else
                       expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                   }
                   else if (input$freqaxisunits == "zero2fsby2") {
                     if ((!input$twosidedFFT) &&
                         (!input$FFTshifted) && (!input$logarithmicFreqAxis)) {
                       expression(0 %->% ~  ~  ~  ~ f[s] ~ ~  ~  ~ "[Hz]")
                     }
                     else {
                       expression(0 %->% ~  ~  ~  ~ f[s] / 2 ~ ~  ~  ~ "[Hz]")
                     }
                   }
                   else if (input$freqaxisunits == "zero2fmax") {
                     expression(0 %->% ~  ~  ~  ~ f[max] ~ ~  ~  ~ "[Hz]")
                   }
                   else if (input$freqaxisunits == "zero2fmaxby2") {
                     if (input$twosidedFFT)
                       expression(-f[max] / 2 %->% ~
                                    ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
                     else
                       expression(0 %->% ~  ~  ~  ~ f[max] / 2 ~ ~  ~  ~ "[Hz]")
                   },
                   ylab = if ((input$freqaxisunits == "zero2one") ||
                              (input$freqaxisunits ==
                               "zero2pi") ||
                              (input$freqaxisunits == "zero22pi") ||
                              (input$freqaxisunits == "zero2half") ||
                              (input$freqaxisunits ==
                               "zero2piby2")) {
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
                 abline(h = 0, col = "black")
                 abline(v = 0, col = "black")
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
                     lwd = 3 *
                       input$LineWidth,
                     lty = "dashed"
                   )
                   box(
                     which = "inner",
                     col = MyColourForUnstableSystem,
                     lwd = 3 *
                       input$LineWidth,
                     lty = "dotted"
                   )
                   grid(col = MyColourForUnstableSystem)
                   abline(h = 0, col = MyColourForUnstableSystem)
                   abline(v = 0, col = MyColourForUnstableSystem)
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
                     lwd = 3 *
                       input$LineWidth,
                     lty = "dashed"
                   )
                   box(
                     which = "inner",
                     col = MyColourForUnstableSystem,
                     lwd = 3 *
                       input$LineWidth,
                     lty = "dotted"
                   )
                   grid(col = MyColourForUnstableSystem)
                   abline(h = 0, col = MyColourForUnstableSystem)
                   abline(v = 0, col = MyColourForUnstableSystem)
                 }
               })
  
  # output$axes_acf ----
  output$axes_acf <- renderPlot(width = "auto", height = "auto", {
    par(mfrow = c(2, 1))
    acf(Re(handleshn()),
        main = "Auto-Correlation Function of Re(h[n])",
        xlim = c(0, if (!is.null(handleshn())) {
          min(length(handleshn()) - 1, 10 * log10(length(handleshn())))
        } else {
          1
        }))
    pacf(Re(handleshn()),
         main = "Partial Auto-Correlation Function of Re(h[n])",
         xlim = c(0, if (!is.null(handleshn())) {
           min(length(handleshn()) - 1, 10 * log10(length(handleshn())))
         } else {
           1
         }))
    par(mfrow = c(1, 1))
  })
  
  # Render Transfer-Function ----
  output$transferfn <- renderUI({
    withMathJax(
      paste0(
        "$$\\begin{align}", # \\label{eq:txfrfn}",
        "H(z) =",
        "\\frac{Y(z)}{X(z)} &= b_0\\cdot\\frac{",
        {
          accumulatorstring <- NULL
          for (i in 1:length(handles$zeroloc)) {
            switch(
              abs(sign(Re(
                handles$zeroloc[i]
              ))) + 1,
              accumulatorstring <- paste0(accumulatorstring,
                                          paste0(if (i > 1) {
                                            "\\cdot "
                                          }, if (abs(Im(handles$zeroloc[i])) <= 1e-06) {
                                            "z"
                                          } else {
                                            paste0(
                                              "(z-\\overbrace{z_{[",
                                              i,
                                              "]}}^{",
                                              gsub("i",
                                                   "\\\\jmath", stripImagZero(round(
                                                     handles$zeroloc[i],
                                                     max(c(
                                                       2,
                                                       countZeroDigitsRightOfDecimalPoint(handles$zeroloc[i])
                                                     ))
                                                   ))),
                                              "})"
                                            )
                                          })),
              accumulatorstring <- paste0(
                accumulatorstring,
                if (i > 1) {
                  "\\cdot "
                },
                "(z-\\overbrace{z_{[",
                i,
                "]}}^{",
                gsub("i", "\\\\jmath",
                     stripImagZero(round(
                       handles$zeroloc[i], max(c(
                         2,
                         countZeroDigitsRightOfDecimalPoint(handles$zeroloc[i])
                       ))
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
          for (i in 1:length(handles$poleloc)) {
            switch(
              abs(sign(Re(
                handles$poleloc[i]
              ))) + 1,
              accumulatorstring <- paste0(accumulatorstring,
                                          if (i > 1) {
                                            "\\cdot "
                                          }, if (abs(Im(
                                            handles$poleloc[i]
                                          )) <= 1e-06) {
                                            "z"
                                          } else {
                                            paste0(
                                              "(z-\\underbrace{p_{[",
                                              i,
                                              "]}}_{",
                                              gsub("i",
                                                   "\\\\jmath", stripImagZero(round(
                                                     handles$poleloc[i],
                                                     max(c(
                                                       2,
                                                       countZeroDigitsRightOfDecimalPoint(handles$poleloc[i])
                                                     ))
                                                   ))),
                                              "})"
                                            )
                                          }),
              accumulatorstring <- paste0(
                accumulatorstring,
                if (i > 1) {
                  "\\cdot "
                },
                "(z-\\underbrace{p_{[",
                i,
                "]}}_{",
                gsub("i", "\\\\jmath",
                     stripImagZero(round(
                       handles$poleloc[i], max(c(
                         2,
                         countZeroDigitsRightOfDecimalPoint(handles$poleloc[i])
                       ))
                     ))),
                "})"
              )
            )
          }
          accumulatorstring
        },
        if ((length(handles$zeroloc) + length(handles$poleloc) >
             8) &&
            (length(handles$poleloc) > 2)) {
          "}\\\\&="
        }
        else {
          "}="
        },
        "\\overbrace{",
        gsub("i", "\\\\jmath", round(input$edit_gain,
                                     max(
                                       c(5, countZeroDigitsRightOfDecimalPoint(input$edit_gain))
                                     ))),
        "}^{G\\equiv b_{[0]}}\\cdot\\frac{",
        {
          accumulatorstring <- NULL
          for (i in 1:length(handles$zeroloc)) {
            switch(
              abs(sign(Re(
                handles$zeroloc[i]
              ))) + 1,
              accumulatorstring <- paste0(accumulatorstring,
                                          if (i > 1) {
                                            "\\cdot "
                                          }, if (abs(Im(
                                            handles$zeroloc[i]
                                          )) <= 1e-06) {
                                            "z"
                                          } else {
                                            paste0(
                                              "(z-\\overbrace{(",
                                              gsub("i", "\\\\jmath",
                                                   stripImagZero(round(
                                                     handles$zeroloc[i], max(c(
                                                       2,
                                                       countZeroDigitsRightOfDecimalPoint(handles$zeroloc[i])
                                                     ))
                                                   ))),
                                              ")}^{z_{[",
                                              i,
                                              "]}",
                                              if (i > 1) {
                                                {
                                                  subaccumulatorstring <- NULL
                                                  for (k in seq(1, (i - 1), by = 1)) {
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
                if (i > 1) {
                  "\\cdot "
                },
                "(z-\\overbrace{(",
                gsub("i", "\\\\jmath", stripImagZero(round(
                  handles$zeroloc[i],
                  max(c(
                    2,
                    countZeroDigitsRightOfDecimalPoint(handles$zeroloc[i])
                  ))
                ))),
                ")}^{z_{[",
                i,
                "]}",
                if (i > 1) {
                  {
                    subaccumulatorstring <- NULL
                    for (k in seq(1, (i - 1), by = 1)) {
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
          for (i in 1:length(handles$poleloc)) {
            switch(
              abs(sign(Re(
                handles$poleloc[i]
              ))) + 1,
              accumulatorstring <- paste0(accumulatorstring,
                                          if (i > 1) {
                                            "\\cdot "
                                          }, if (abs(Im(
                                            handles$poleloc[i]
                                          )) <= 1e-06) {
                                            "z"
                                          } else {
                                            paste0(
                                              "(z-\\underbrace{(",
                                              gsub("i", "\\\\jmath",
                                                   stripImagZero(round(
                                                     handles$poleloc[i], max(c(
                                                       2,
                                                       countZeroDigitsRightOfDecimalPoint(handles$poleloc[i])
                                                     ))
                                                   ))),
                                              ")}_{p_{[",
                                              i,
                                              "]}",
                                              if (i > 1) {
                                                subaccumulatorstring <- NULL
                                                for (k in seq(1, (i - 1), by = 1)) {
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
                if (i > 1) {
                  "\\cdot "
                },
                "(z-\\underbrace{(",
                gsub("i", "\\\\jmath", stripImagZero(round(
                  handles$poleloc[i],
                  max(c(
                    2,
                    countZeroDigitsRightOfDecimalPoint(handles$poleloc[i])
                  ))
                ))),
                ")}_{p_{[",
                i,
                "]}",
                if (i > 1) {
                  subaccumulatorstring <- NULL
                  for (k in seq(1, (i - 1), by = 1)) {
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
        "}\\\\",
        "H(z) &=",
        if ((length(handles$zeroloc) <= 2) &&
            (length(handles$poleloc) <= 2)) {
          paste0(
            "\\overbrace{",
            gsub("i", "\\\\jmath", round(input$edit_gain,
                                         max(
                                           c(
                                             5,
                                             countZeroDigitsRightOfDecimalPoint(input$edit_gain)
                                           )
                                         ))),
            "}^{G\\equiv b_{[0]}}\\cdot\\frac{",
            "1",
            if (length(handlesb()) >
                1) {
              paste0(switch(
                abs(sign(Re(
                  handles$zeroloc[1]
                ))) + 1,
                if (abs(Im(handles$zeroloc[1])) <= 1e-06) {
                  "+(0\\cdot z^{-1})"
                } else {
                  paste0(
                    "+ \\overbrace{\\left[-(",
                    gsub("i", "\\\\jmath",
                         stripImagZero(round(
                           handles$zeroloc[1], max(c(
                             2,
                             countZeroDigitsRightOfDecimalPoint(handles$zeroloc[1])
                           ))
                         ))),
                    ")",
                    if (length(handles$zeroloc) > 1) {
                      paste0("-(", gsub("i", "\\\\jmath", stripImagZero(
                        round(handles$zeroloc[2],
                              max(
                                c(
                                  2,
                                  countZeroDigitsRightOfDecimalPoint(handles$zeroloc[2])
                                )
                              ))
                      )),
                      ")")
                    },
                    "\\right]}^{b_{[1]}/b_{[0]}=",
                    gsub("i", "\\\\jmath",
                         stripImagZero(
                           round(handlesb()[2] / handlesb()[1],
                                 max(
                                   c(
                                     2,
                                     countZeroDigitsRightOfDecimalPoint(handlesb()[2] / handlesb()[1])
                                   )
                                 ))
                         )),
                    "}\\cdot z^{-1}"
                  )
                },
                paste0(
                  "+ \\overbrace{\\left[-(",
                  gsub("i", "\\\\jmath",
                       stripImagZero(round(
                         handles$zeroloc[1], max(c(
                           2,
                           countZeroDigitsRightOfDecimalPoint(handles$zeroloc[1])
                         ))
                       ))),
                  ")",
                  if (length(handles$zeroloc) > 1) {
                    paste0("-(", gsub("i", "\\\\jmath", stripImagZero(round(
                      handles$zeroloc[2],
                      max(c(
                        2,
                        countZeroDigitsRightOfDecimalPoint(handles$zeroloc[2])
                      ))
                    ))),
                    ")")
                  },
                  "\\right]}^{b_{[1]}/b_{[0]}=",
                  gsub("i", "\\\\jmath",
                       stripImagZero(
                         round(handlesb()[2] / handlesb()[1],
                               max(
                                 c(
                                   2,
                                   countZeroDigitsRightOfDecimalPoint(handlesb()[2] / handlesb()[1])
                                 )
                               ))
                       )),
                  "}\\cdot z^{-1}"
                )
              ), if (length(handlesb()) > 2) {
                switch(
                  abs(sign(Re(
                    handlesb()[3] / handlesb()[1]
                  ))) +
                    1,
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
                                  2,
                                  countZeroDigitsRightOfDecimalPoint(handles$zeroloc[i])
                                )
                              ))
                      )),
                      ")}^{b_{[2]}/b_{[0]}=",
                      gsub("i", "\\\\jmath",
                           stripImagZero(
                             round(handlesb()[3] / handlesb()[1],
                                   max(
                                     c(
                                       2,
                                       countZeroDigitsRightOfDecimalPoint(handlesb()[2 +
                                                                                       1] /
                                                                            handlesb()[1])
                                     )
                                   ))
                           )),
                      "}\\cdot z^{-2}"
                    )
                  },
                  paste0(
                    "+ \\overbrace{(",
                    gsub("i", "\\\\jmath",
                         stripImagZero(round(
                           handles$zeroloc[1], max(c(
                             2,
                             countZeroDigitsRightOfDecimalPoint(handles$zeroloc[1])
                           ))
                         ))),
                    ")",
                    "\\cdot (",
                    gsub("i", "\\\\jmath", stripImagZero(round(
                      handles$zeroloc[2],
                      max(c(
                        2,
                        countZeroDigitsRightOfDecimalPoint(handles$zeroloc[i])
                      ))
                    ))),
                    ")}^{b_{[2]}/b_{[0]}=",
                    gsub("i", "\\\\jmath",
                         stripImagZero(
                           round(handlesb()[3] / handlesb()[1],
                                 max(
                                   c(
                                     2,
                                     countZeroDigitsRightOfDecimalPoint(handlesb()[i +
                                                                                     1] /
                                                                          handlesb()[1])
                                   )
                                 ))
                         )),
                    "}\\cdot z^{-2}"
                  )
                )
              })
            },
            "}{",
            "1",
            if (length(handlesa()) > 1) {
              paste0(switch(
                abs(sign(Re(
                  handles$poleloc[1]
                ))) + 1,
                if (abs(Im(handles$poleloc[1])) <= 1e-06) {
                  "-(0\\cdot z^{-1})"
                } else {
                  paste0(
                    "\\underbrace{-\\left[(",
                    gsub("i", "\\\\jmath",
                         stripImagZero(round(
                           handles$poleloc[1], max(c(
                             2,
                             countZeroDigitsRightOfDecimalPoint(handles$poleloc[1])
                           ))
                         ))),
                    ")",
                    if (length(handles$poleloc) > 1) {
                      paste0("+(", gsub("i", "\\\\jmath", stripImagZero(
                        round(handles$poleloc[2],
                              max(
                                c(
                                  2,
                                  countZeroDigitsRightOfDecimalPoint(handles$poleloc[2])
                                )
                              ))
                      )),
                      ")")
                    },
                    "\\right]}_{a_{[1]}=",
                    gsub("i", "\\\\jmath",
                         stripImagZero(round(
                           handlesa()[2], max(c(
                             2,
                             countZeroDigitsRightOfDecimalPoint(handlesa()[2])
                           ))
                         ))),
                    "}\\cdot z^{-1}"
                  )
                },
                paste0(
                  "\\underbrace{-\\left[(",
                  gsub("i", "\\\\jmath",
                       stripImagZero(round(
                         handles$poleloc[1], max(c(
                           2,
                           countZeroDigitsRightOfDecimalPoint(handles$poleloc[1])
                         ))
                       ))),
                  ")",
                  if (length(handles$poleloc) > 1) {
                    paste0("+(", gsub("i", "\\\\jmath", stripImagZero(round(
                      handles$poleloc[2],
                      max(c(
                        2,
                        countZeroDigitsRightOfDecimalPoint(handles$poleloc[2])
                      ))
                    ))),
                    ")")
                  },
                  "\\right]}_{a_{[1]}=",
                  gsub("i", "\\\\jmath",
                       stripImagZero(round(
                         handlesa()[2], max(c(
                           2, countZeroDigitsRightOfDecimalPoint(handlesa()[2])
                         ))
                       ))),
                  "}\\cdot z^{-1}"
                )
              ), if (length(handlesa()) > 2) {
                switch(
                  abs(sign(Re(
                    handlesa()[3]
                  ))) + 1,
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
                                 2,
                                 countZeroDigitsRightOfDecimalPoint(handles$poleloc[1])
                               )
                             ))
                           )),
                      ")",
                      "\\cdot (",
                      gsub("i", "\\\\jmath", stripImagZero(
                        round(handles$poleloc[2],
                              max(
                                c(
                                  2,
                                  countZeroDigitsRightOfDecimalPoint(handles$poleloc[i])
                                )
                              ))
                      )),
                      ")}_{a_{[2]}=",
                      gsub("i", "\\\\jmath", stripImagZero(round(
                        handlesa()[3],
                        max(c(
                          2,
                          countZeroDigitsRightOfDecimalPoint(handlesa()[i +
                                                                          1])
                        ))
                      ))),
                      "}\\cdot z^{-2}"
                    )
                  },
                  paste0(
                    "\\underbrace{+(",
                    gsub("i", "\\\\jmath",
                         stripImagZero(round(
                           handles$poleloc[1], max(c(
                             2,
                             countZeroDigitsRightOfDecimalPoint(handles$poleloc[1])
                           ))
                         ))),
                    ")",
                    "\\cdot (",
                    gsub("i", "\\\\jmath", stripImagZero(round(
                      handles$poleloc[2],
                      max(c(
                        2,
                        countZeroDigitsRightOfDecimalPoint(handles$poleloc[i])
                      ))
                    ))),
                    ")}_{a_{[2]}=",
                    gsub("i", "\\\\jmath", stripImagZero(round(
                      handlesa()[3],
                      max(c(
                        2, countZeroDigitsRightOfDecimalPoint(handlesa()[i +
                                                                           1])
                      ))
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
              5,
              countZeroDigitsRightOfDecimalPoint(input$edit_gain)
            )
          )),
          "\\cdot")
        },
        "\\frac{",
        {
          polyproduct <- 1
          for (i in 1:length(handles$zeroloc)) {
            polyproduct <-
              pracma::polymul(polyproduct, c(1, handles$zeroloc[i]))
          }
          numeratorstring <-
            if (max(Mod(polyproduct)) > 1e+06) {
              "0"
            }
          else {
            "1"
          }
          if (length(polyproduct) > 1) {
            for (i in 2:length(polyproduct)) {
              switch(
                sign(Re(polyproduct[i])) + 2,
                numeratorstring <- paste0(numeratorstring,
                                          switch((i %%
                                                    2) + 1, "+", "-"), if (polyproduct[i] ==
                                                                           1) {
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
                                                          -polyproduct[i] / if (max(Mod(polyproduct)) > 1e+06) {
                                                            max(Mod(polyproduct))
                                                          } else {
                                                            1
                                                          }
                                                        }, max(c(
                                                          2,
                                                          countZeroDigitsRightOfDecimalPoint(polyproduct[i])
                                                        ))
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
                                                    }, "z^{-", i - 1, "}"),
                "",
                numeratorstring <- paste0(numeratorstring,
                                          switch((i %%
                                                    2) + 1, "-", "+"), if (polyproduct[i] ==
                                                                           -1) {
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
                                                          polyproduct[i] / if (max(Mod(polyproduct)) > 1e+06) {
                                                            max(Mod(polyproduct))
                                                          } else {
                                                            1
                                                          }
                                                        }, max(c(
                                                          2,
                                                          countZeroDigitsRightOfDecimalPoint(polyproduct[i])
                                                        ))
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
                                                    }, "z^{-", i - 1, "}")
              )
            }
          }
          numeratorstring
        },
        "}{",
        {
          polyproduct <- 1
          for (i in 1:length(handles$poleloc)) {
            polyproduct <-
              pracma::polymul(polyproduct, c(1, handles$poleloc[i]))
          }
          denominatorstring <-
            if (max(Mod(polyproduct)) > 1e+06) {
              "0"
            }
          else {
            "1"
          }
          if (length(polyproduct) > 1) {
            for (i in 2:length(polyproduct)) {
              switch(
                sign(Re(polyproduct[i])) + 2,
                denominatorstring <- paste0(
                  denominatorstring,
                  switch((i %%
                            2) + 1, "+", "-"),
                  if (polyproduct[i] ==
                      1) {
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
                        -polyproduct[i] / if (max(Mod(polyproduct)) > 1e+06) {
                          max(Mod(polyproduct))
                        } else {
                          1
                        }
                      }, max(c(
                        2,
                        countZeroDigitsRightOfDecimalPoint(polyproduct[i])
                      ))
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
                  i - 1,
                  "}"
                ),
                "",
                denominatorstring <- paste0(
                  denominatorstring,
                  switch((i %%
                            2) + 1, "-", "+"),
                  if (polyproduct[i] ==
                      -1) {
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
                        polyproduct[i] / if (max(Mod(polyproduct)) > 1e+06) {
                          max(Mod(polyproduct))
                        } else {
                          1
                        }
                      }, max(c(
                        2,
                        countZeroDigitsRightOfDecimalPoint(polyproduct[i])
                      ))
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
                  i - 1,
                  "}"
                )
              )
            }
          }
          denominatorstring
        },
        "}\\end{align}$$"
      )
    )
  })

  # Render Difference-Equation ----
  output$diffeqn <- renderUI({
    withMathJax(
      paste0(
        "$$\\begin{align}", # \\label{eq:diffeqn}",
        "y_{[n]}=",
        "\\left(",
        "\\sum_{k^{\\prime}=1}^{M}{a_{[k^{\\prime}]}\\cdot y_{[n-k^{\\prime}]}}",
        "\\right)",
        "&+\\left(",
        "\\sum_{k=0}^{N}{b_{[k]} \\cdot x_{[n-k]}}",
        "\\right)",
        "\\\\",
        "y_{[n]} =",
        {
          accumulatorstring <- NULL
          for (i in 2:length(handlesa())) {
            if (i > 2) {
              accumulatorstring <- paste0(accumulatorstring, "+")
            }
            switch(
              abs(sign(Re(
                handlesa()[i]
              ))) + 1,
              accumulatorstring <- paste0(accumulatorstring,
                                          if (Im(handlesa()[i])) {
                                            paste0("(0\\cdot y_{[n-", i - 1, "]})")
                                          } else {
                                            paste0(
                                              "\\underbrace{(",
                                              gsub("i", "\\\\jmath", stripImagZero(round(
                                                handlesa()[i],
                                                max(c(
                                                  2, countZeroDigitsRightOfDecimalPoint(handlesa()[i])
                                                ))
                                              ))),
                                              ")}_{a_{[",
                                              i - 1,
                                              "]}}\\cdot y_{[n-",
                                              i - 1,
                                              "]}"
                                            )
                                          }),
              accumulatorstring <- paste0(
                accumulatorstring,
                "\\underbrace{(",
                gsub("i", "\\\\jmath", stripImagZero(round(
                  handlesa()[i],
                  max(c(
                    2, countZeroDigitsRightOfDecimalPoint(handlesa()[i])
                  ))
                ))),
                ")}_{a_{[",
                i - 1,
                "]}}\\cdot y_{[n-",
                i - 1,
                "]}"
              )
            )
          }
          accumulatorstring
        },
        switch(
          abs(sign(Re(1))) + 1,
          "&+(0\\cdot x_{[n]})",
          paste0(
            "&+\\underbrace{",
            gsub("i", "\\\\jmath", 1),
            "}_{b_{[0]}/b_{[0]}}\\cdot x_{[n]}"
          )
        ),
        {
          accumulatorstring <- NULL
          for (i in 2:length(handlesb())) {
            switch(
              abs(sign(Re(
                handlesb()[i] / handlesb()[1]
              ))) + 1,
              accumulatorstring <-
                paste0(accumulatorstring, if (abs(Im(
                  handlesb()[i] / handlesb()[1]
                )) <=
                1e-06) {
                  paste0("+(0\\cdot x_{[n-", i - 1, "]})")
                } else {
                  paste0(
                    "+ \\underbrace{(",
                    gsub("i", "\\\\jmath",
                         stripImagZero(
                           round(handlesb()[i] / handlesb()[1],
                                 max(
                                   c(
                                     2,
                                     countZeroDigitsRightOfDecimalPoint(handlesb()[i] / handlesb()[1])
                                   )
                                 ))
                         )),
                    ")}_{b_{[",
                    i - 1,
                    "]}/b_{[0]}}\\cdot x_{[n-",
                    i - 1,
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
                            2,
                            countZeroDigitsRightOfDecimalPoint(handlesb()[i] / handlesb()[1])
                          )
                        ))
                )),
                ")}_{b_{[",
                i - 1,
                "]}/b_{[0]}}\\cdot x_{[n-",
                i -
                  1,
                "]}"
              )
            )
          }
          accumulatorstring
        },
        "\\\\",
        "\\underbrace{}_{a_{[0]}\\equiv1}y_{[n]}",
        {
          accumulatorstring <- NULL
          for (i in 2:length(handlesa())) {
            switch(
              abs(sign(Re(
                handlesa()[i]
              ))) + 1,
              accumulatorstring <- paste0(accumulatorstring,
                                          if (Im(handlesa()[i])) {
                                            paste0("+(\\underbrace{0}_{a_{[",
                                                   i - 1,
                                                   "]}}\\cdot y_{[n-",
                                                   i - 1,
                                                   "]})")
                                          } else {
                                            paste0(
                                              "\\underbrace{-",
                                              "\\underbrace{(",
                                              gsub("i",
                                                   "\\\\jmath", stripImagZero(round(
                                                     handlesa()[i],
                                                     max(c(
                                                       2, countZeroDigitsRightOfDecimalPoint(handlesa()[i])
                                                     ))
                                                   ))),
                                              ")}_{a_{[",
                                              i - 1,
                                              "]}}}_{-a_{[",
                                              i - 1,
                                              "]}=",
                                              gsub("i", "\\\\jmath", stripImagZero(round(
                                                -handlesa()[i],
                                                max(c(
                                                  2,
                                                  countZeroDigitsRightOfDecimalPoint(-handlesa()[i])
                                                ))
                                              ))),
                                              "}\\cdot y_{[n-",
                                              i - 1,
                                              "]}"
                                            )
                                          }),
              accumulatorstring <- paste0(
                accumulatorstring,
                "\\underbrace{-",
                "\\underbrace{(",
                gsub("i", "\\\\jmath",
                     stripImagZero(round(
                       handlesa()[i], max(c(
                         2, countZeroDigitsRightOfDecimalPoint(handlesa()[i])
                       ))
                     ))),
                ")}_{a_{[",
                i - 1,
                "]}}}_{-a_{[",
                i - 1,
                "]}=",
                gsub("i",
                     "\\\\jmath", stripImagZero(round(
                       -handlesa()[i],
                       max(c(
                         2, countZeroDigitsRightOfDecimalPoint(-handlesa()[i])
                       ))
                     ))),
                "}\\cdot y_{[n-",
                i - 1,
                "]}"
              )
            )
          }
          accumulatorstring
        },
        "&=",
        switch(
          abs(sign(Re(1))) + 1,
          "(\\underbrace{0}_{b_{[0]}/b_{[0]}}\\cdot x_{[n]})",
          paste0(
            "\\underbrace{",
            gsub("i", "\\\\jmath", if (max(Mod(handlesb(
            ))) >
            1e+06) {
              0
            } else {
              1
            }),
            "}_{b_{[0]}/b_{[0]}}\\cdot x_{[n]}"
          )
        ),
        {
          accumulatorstring <- NULL
          for (i in 2:length(handlesb())) {
            switch(
              abs(sign(Re(
                handlesb()[i] / handlesb()[1]
              ))) + 1,
              accumulatorstring <-
                paste0(accumulatorstring, if (Re(handlesb()[i] / handlesb()[1])) {
                  paste0("+(\\underbrace{0}_{b_{[",
                         i - 1,
                         "]}/b_{[0]}}\\cdot x_{[n-",
                         i - 1,
                         "]})")
                } else {
                  paste0(
                    "+ \\underbrace{(",
                    gsub("i", "\\\\jmath",
                         stripImagZero(round(
                           if (max(Mod(handlesb())) >
                               1e+06) {
                             (handlesb()[i] / handlesb()[1]) / (1 / eps) / 1000
                           } else {
                             handlesb()[i] / handlesb()[1]
                           }, max(c(
                             2,
                             countZeroDigitsRightOfDecimalPoint(handlesb()[i] / handlesb()[1])
                           ))
                         ))),
                    ")}_{b_{[",
                    i - 1,
                    "]}/b_{[0]}}\\cdot x_{[n-",
                    i - 1,
                    "]}"
                  )
                }),
              accumulatorstring <- paste0(
                accumulatorstring,
                "+ \\underbrace{(",
                gsub("i", "\\\\jmath", stripImagZero(round(
                  if (max(Mod(handlesb())) >
                      1e+06) {
                    (handlesb()[i] / handlesb()[1]) / (1 / eps) / 1000
                  } else {
                    handlesb()[i] / handlesb()[1]
                  }, max(c(
                    2,
                    countZeroDigitsRightOfDecimalPoint(handlesb()[i] / handlesb()[1])
                  ))
                ))),
                ")}_{b_{[",
                i - 1,
                "]}/b_{[0]}}\\cdot x_{[n-",
                i -
                  1,
                "]}"
              )
            )
          }
          accumulatorstring
        },
        "\\end{align}$$"
      )
    )
  })
  
  # output$rect2polarPZplot Text ----
  output$rect2polarPZplot <- renderText({
    shiny::req(input$edit_polezeroloc, input$edit_polezerolocImag)
    if ((grepl("rnorm|runif", input$edit_polezeroloc)) ||
        (grepl("rnorm|runif",
               input$edit_polezerolocImag))) {
      invalidateLater(10000)
    }
    n1 <- eval(parse(text = input$edit_polezeroloc))
    n2 <- eval(parse(text = input$edit_polezerolocImag))
    if (is.infinite(n2)) {
      a <- Re(n1)
      b <- n2
    }
    else {
      a <- Re(n1 + j * n2)
      b <- Im(n1 + j * n2)
    }
    Mytheta <- atan2(b, a)
    paste0(
      "radius: ",
      round(sqrt(a * a + b * b), 3),
      ", ",
      "theta: ",
      round(Mytheta, 3),
      if (Mytheta != 0) {
        paste0("= ", switch(sign(Mytheta) + 2, "-", "", ""), "pi",
               "/", round(abs(pi / Mytheta), 1))
      },
      " rads",
      " (",
      round(Mytheta * 180 / pi, 0),
      " deg)"
    )
  })
  output$polar2rectPZplot <- renderText({
    if ((grepl("rnorm|runif", input$edit_polezerolocRadius)) ||
        (grepl("rnorm|runif",
               input$edit_polezerolocAngle))) {
      invalidateLater(10000)
    }
    r <- eval(parse(text = input$edit_polezerolocRadius))
    theta <- eval(parse(text = input$edit_polezerolocAngle))
    a <- r * exp(j * theta)
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
      n1 <- eval(parse(text = input$edit_polezerolocRadius))
      n2 <- eval(parse(text = input$edit_polezerolocAngle))
      valueToBeAdded <- n1 * exp(n2)
      if (is.infinite(Im(n2))) {
        valueToBeAdded <- complex(1, n1, switch(sign(n2) + 2,-Inf,
                                                0, Inf))
      }
      else if (is.infinite(Re(n2))) {
        valueToBeAdded <- complex(1, n1, switch(sign(n2) + 2,-Inf,
                                                0, Inf))
      }
      else {
        if ((is.null(input$edit_currentSelectionText)) ||
            (is.na(input$edit_currentSelectionText)) ||
            (input$edit_currentSelectionText == "")) {
          valueToBeAdded <- n1 * exp(j * n2)
        }
        else {
          valueToBeAdded <-
            eval(parse(text = input$edit_currentSelectionText))
        }
      }
      if (Mod(valueToBeAdded) > 1e-06) {
        if (abs(Im(valueToBeAdded)) == 0) {
          valueToBeAdded <- Re(valueToBeAdded)
        }
        shinyBS::closeAlert(session, alertId = "noMorePoles")
        shinyBS::closeAlert(session, alertId = "noMoreZeros")
        if ((is.null(handles$poleloc)) ||
            (is.na(handles$poleloc)) ||
            ((length(handles$poleloc) == 1) &&
             (handles$poleloc[1] ==
              0))) {
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
          ui = "Value is too small...",
          duration = 3,
          closeButton = TRUE,
          type = "message"
        )
      }
    }
    else if (input$tabPoleZeroEditing == "RealImag") {
      n1 <- eval(parse(text = input$edit_polezeroloc))
      n2 <- eval(parse(text = input$edit_polezerolocImag))
      if (is.infinite(Im(n2))) {
        valueToBeAdded <- complex(1, n1, switch(sign(n2) + 2,-Inf,
                                                0, Inf))
      }
      else if (is.infinite(Re(n2))) {
        valueToBeAdded <- complex(1, n1, switch(sign(n2) + 2,-Inf,
                                                0, Inf))
      }
      else {
        if ((is.null(input$edit_currentSelectionText)) ||
            (is.na(input$edit_currentSelectionText)) ||
            (input$edit_currentSelectionText == "")) {
          valueToBeAdded <- n1 + j * n2
        }
        else {
          valueToBeAdded <-
            eval(parse(text = input$edit_currentSelectionText))
        }
      }
      if ((Mod(valueToBeAdded) > 1e-06)) {
        shinyBS::closeAlert(session, alertId = "noMorePoles")
        shinyBS::closeAlert(session, alertId = "noMoreZeros")
        if (abs(Im(valueToBeAdded)) == 0) {
          valueToBeAdded <- Re(valueToBeAdded)
        }
        if ((is.null(handles$poleloc)) ||
            (is.na(handles$poleloc)) ||
            ((length(handles$poleloc) == 1) &&
             (handles$poleloc[1] ==
              0))) {
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
          ui = "Value is too small...",
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
        zpg <- eval(parse(text = inputFilterCommandString))
        handles$zeroloc <- zpg$zero
        handles$poleloc <- zpg$pole
        updateNumericInput(session, inputId = "edit_gain", value = zpg$gain)
        b <- zpg$gain
        length(b) <- 1
      }
      else if (grepl(
        "fir1|fir2|remez|spencerFilter|sgolayfilt|sgolay|Ma|chebwin|kaiser|bartlett|blackman|boxcar|flattopwin|gausswin|hanning|hamming|triang|sinc",
        inputFilterCommandString
      )) {
        handles$poleloc <- c(0)
        b <- eval(parse(text = inputFilterCommandString))
        if (length(b) < 2) {
          handles$zeroloc <- c(0)
        }
        else {
          handles$zeroloc <- polyroot(rev(b)) # numerical stability may be an issue for all but low-degree polynomials
        }
      }
      else if (grepl("FftFilter", inputFilterCommandString)) {
        handles$poleloc <- c(0)
        b <- eval(parse(text = inputFilterCommandString))
        if (length(b) < 2) {
          handles$zeroloc <- c(0)
        }
        else {
          handles$zeroloc <- polyroot(rev(b)) # numerical stability may be an issue for all but low-degree polynomials
        }
      }
      else {
        a <- eval(parse(text = paste0(inputFilterCommandString,
                                      "$a")))
        if (length(a) < 2) {
          handles$poleloc <- c(0)
        }
        else {
          handles$poleloc <- polyroot(rev(a)) # numerical stability may be an issue for all but low-degree polynomials
        }
        b <- eval(parse(text = paste0(inputFilterCommandString,
                                      "$b")))
        if (length(b) < 2) {
          handles$zeroloc <- c(0)
        }
        else {
          handles$zeroloc <- polyroot(rev(b)) # numerical stability may be an issue for all but low-degree polynomials
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
        rawList <- readLines(con = inpFile$datapath)
        Npoles <- match("#zerolocs", rawList) - match("#polelocs",
                                                      rawList) - 1
        importedPoles <- scan(
          file = inpFile$datapath,
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
          for (i in 1:length(importedPoles)) {
            importedPolesValues[i] <- eval(parse(text = importedPoles[i]))
          }
        }
        else {
          importedPolesValues <- importedPoles
        }
        handles$poleloc <- importedPolesValues
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
        importedZeros <-
          scan(
            file = inpFile$datapath,
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
          for (i in 1:length(importedZeros)) {
            importedZerosValues[i] <- eval(parse(text = importedZeros[i]))
          }
        }
        else {
          importedZerosValues <- importedZeros
        }
        handles$zeroloc <- importedZerosValues
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
      }
      else if (input$textbinaryformatImport == "bin") {
        
      }
      else if (input$textbinaryformatImport == "RData") {
        
      }
      else if (input$textbinaryformatImport == "mat") {
        
      }
      else if (input$textbinaryformatImport == "yml") {
        
      }
    }
  })
  
  # observeEvent (button) addzero ----
  observeEvent(eventExpr = input$pb_addzero, handlerExpr = {
    if (input$tabPoleZeroEditing == "rtheta") {
      n1 <- eval(parse(text = input$edit_polezerolocRadius))
      n2 <- eval(parse(text = input$edit_polezerolocAngle))
      if (n2 > 2 * pi) {
        n2 <- 2 * pi
      }
      if (n2 < -2 * pi) {
        n2 <- -2 * pi
      }
      if ((is.null(input$edit_currentSelectionText)) ||
          (is.na(input$edit_currentSelectionText)) ||
          (input$edit_currentSelectionText == "")) {
        valueToBeAdded <- n1 * exp(j * n2)
      }
      else {
        valueToBeAdded <-
          eval(parse(text = input$edit_currentSelectionText))
      }
      if ((!is.null(valueToBeAdded)) && (!is.na(valueToBeAdded)) &&
          (Im(valueToBeAdded) == 0)) {
        valueToBeAdded <- Re(valueToBeAdded)
      }
      if (valueToBeAdded != 0) {
        shinyBS::closeAlert(session, alertId = "noMoreZeros")
        shinyBS::closeAlert(session, alertId = "noMorePoles")
        if ((is.null(handles$zeroloc)) ||
            (is.na(handles$zeroloc)) ||
            ((length(handles$zeroloc) == 1) &&
             (handles$zeroloc[1] ==
              0))) {
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
      n1 <- eval(parse(text = input$edit_polezeroloc))
      n2 <- eval(parse(text = input$edit_polezerolocImag))
      if (is.infinite(Im(n2))) {
        valueToBeAdded <- complex(1, n1, switch(sign(n2) + 2,-Inf,
                                                0, Inf))
      }
      else if (is.infinite(Re(n2))) {
        valueToBeAdded <- complex(1, n1, switch(sign(n2) + 2,-Inf,
                                                0, Inf))
      }
      else {
        if ((is.null(input$edit_currentSelectionText)) ||
            (is.na(input$edit_currentSelectionText)) ||
            (input$edit_currentSelectionText == "")) {
          valueToBeAdded <- n1 + j * n2
        }
        else {
          valueToBeAdded <-
            eval(parse(text = input$edit_currentSelectionText))
        }
      }
      if (Mod(valueToBeAdded) > 1e-06) {
        shinyBS::closeAlert(session, alertId = "noMoreZeros")
        shinyBS::closeAlert(session, alertId = "noMorePoles")
        if (abs(Im(valueToBeAdded)) == 0) {
          valueToBeAdded <- Re(valueToBeAdded)
        }
        if ((is.null(handles$zeroloc)) ||
            (is.na(handles$zeroloc)) ||
            ((length(handles$zeroloc) == 1) &&
             (handles$zeroloc[1] ==
              0))) {
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
          ui = "Value is too small...",
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
        zpg <- eval(parse(text = inputFilterCommandString))
        handles$zeroloc <- zpg$zero
        handles$poleloc <- zpg$pole
        updateNumericInput(session, inputId = "edit_gain", value = zpg$gain)
        b <- zpg$gain
        length(b) <- 1
      }
      else if (grepl(
        "fir1|fir2|remez|spencerFilter|sgolayfilt|sgolay|Ma|chebwin|kaiser|bartlett|blackman|boxcar|flattopwin|gausswin|hanning|hamming|triang|sinc",
        inputFilterCommandString
      )) {
        handles$poleloc <- c(0)
        b <- eval(parse(text = inputFilterCommandString))
        if (length(b) < 2) {
          handles$zeroloc <- c(0)
        }
        else {
          handles$zeroloc <- polyroot(rev(b)) # numerical stability may be an issue for all but low-degree polynomials
        }
      }
      else if (grepl("FftFilter", inputFilterCommandString)) {
        handles$poleloc <- c(0)
        b <- eval(parse(text = inputFilterCommandString))
        if (length(b) < 2) {
          handles$zeroloc <- c(0)
        }
        else {
          handles$zeroloc <- polyroot(rev(b)) # numerical stability may be an issue for all but low-degree polynomials
        }
      }
      else {
        a <- eval(parse(text = paste0(inputFilterCommandString, "$a")))
        if (length(a) < 2) {
          handles$poleloc <- c(0)
        }
        else {
          handles$poleloc <- polyroot(rev(a)) # numerical stability may be an issue for all but low-degree polynomials
        }
        b <- eval(parse(text = paste0(inputFilterCommandString, "$b")))
        if (length(b) < 2) {
          handles$zeroloc <- c(0)
        }
        else {
          handles$zeroloc <- polyroot(rev(b)) # numerical stability may be an issue for all but low-degree polynomials
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
        rawList <- readLines(con = inpFile$datapath)
        Npoles <- match("#zerolocs", rawList) - match("#polelocs",
                                                      rawList) - 1
        importedPoles <- scan(
          file = inpFile$datapath,
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
          for (i in 1:length(importedPoles)) {
            importedPolesValues[i] <- eval(parse(text = importedPoles[i]))
          }
        }
        else {
          importedPolesValues <- importedPoles
        }
        handles$poleloc <- importedPolesValues
        updateSelectInput(session, "listbox_pole", choices = handles$poleloc)
        importedZeros <-
          scan(
            file = inpFile$datapath,
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
          for (i in 1:length(importedZeros)) {
            importedZerosValues[i] <- eval(parse(text = importedZeros[i]))
          }
        }
        else {
          importedZerosValues <- importedZeros
        }
        handles$zeroloc <- importedZerosValues
        updateSelectInput(session, "listbox_zero", choices = handles$zeroloc)
      }
      else if (input$textbinaryformatImport == "bin") {
        
      }
      else if (input$textbinaryformatImport == "RData") {
        
      }
      else if (input$textbinaryformatImport == "mat") {
        
      }
      else if (input$textbinaryformatImport == "yml") {
        
      }
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
        (input$tabPoleZeroEditing ==
         "rtheta")) {
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
      evaluatedinputlistbox_pole <- eval(parse(text = input$listbox_pole))
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
             (input$tabPoleZeroEditing ==
              "Import") ||
             (input$tabPoleZeroEditing == "Export")) {
      updateTabsetPanel(session, inputId = "tabPoleZeroEditing",
                        selected = "RealImag")
      updateTextInput(session,
                      inputId = "edit_currentSelectionText",
                      value = input$listbox_pole)
    }
  })
  observeEvent(eventExpr = input$listbox_zero, handlerExpr = {
    if ((input$tabPoleZeroEditing == "RealImag") ||
        (input$tabPoleZeroEditing ==
         "rtheta")) {
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
      evaluatedinputlistbox_zero <- eval(parse(text = input$listbox_zero))
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
             (input$tabPoleZeroEditing ==
              "Import") ||
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
            valueToReplace <-
              eval(parse(text = input$edit_polezerolocRadius)) *
              exp(j * eval(parse(
                text = input$edit_polezerolocAngle
              )))
          }
          else {
            valueToReplace <-
              eval(parse(text = input$edit_currentSelectionText))
          }
        }
        else {
          if ((is.null(input$edit_currentSelectionText)) ||
              (is.na(input$edit_currentSelectionText)) ||
              (input$edit_currentSelectionText == "")) {
            n1 <- eval(parse(text = input$edit_polezeroloc))
            n2 <- eval(parse(text = input$edit_polezerolocImag))
            if (is.infinite(Im(n2))) {
              valueToReplace <- complex(1, n1, switch(sign(n2) +
                                                        2,-Inf, 0, Inf))
            }
            else if (is.infinite(Re(n2))) {
              valueToReplace <- complex(1, n1, switch(sign(n2) +
                                                        2,-Inf, 0, Inf))
            }
            else {
              valueToReplace <- n1 + j * n2
            }
          }
          else {
            valueToReplace <-
              eval(parse(text = input$edit_currentSelectionText))
          }
        }
      }
      if ((Mod(valueToReplace) > 1e-06)) {
        if (abs(Im(valueToReplace)) == 0) {
          valueToReplace <- Re(valueToReplace)
        }
        handles$poleloc[matchpoint] <- valueToReplace
        updateSelectInput(session,
                          inputId = "listbox_pole",
                          choices = handles$poleloc)
      }
      else {
        showNotification(
          ui = "Value is too small...",
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
            valueToReplace <-
              eval(parse(text = input$edit_polezerolocRadius)) *
              exp(j * eval(parse(
                text = input$edit_polezerolocAngle
              )))
          }
          else {
            valueToReplace <-
              eval(parse(text = input$edit_currentSelectionText))
          }
        }
        else {
          if ((is.null(input$edit_currentSelectionText)) ||
              (is.na(input$edit_currentSelectionText)) ||
              (input$edit_currentSelectionText == "")) {
            n1 <- eval(parse(text = input$edit_polezeroloc))
            n2 <- eval(parse(text = input$edit_polezerolocImag))
            if (is.infinite(Im(n2))) {
              valueToReplace <- complex(1, n1, switch(sign(n2) +
                                                        2,-Inf, 0, Inf))
            }
            else if (is.infinite(Re(n2))) {
              valueToReplace <- complex(1, n1, switch(sign(n2) +
                                                        2,-Inf, 0, Inf))
            }
            else {
              valueToReplace <- n1 + j * n2
            }
          }
          else {
            valueToReplace <-
              eval(parse(text = input$edit_currentSelectionText))
          }
        }
      }
      if ((Mod(valueToReplace) > 1e-06)) {
        if (abs(Im(valueToReplace)) == 0) {
          valueToReplace <- Re(valueToReplace)
        }
        handles$zeroloc[matchpoint] <- valueToReplace
        updateSelectInput(session,
                          inputId = "listbox_zero",
                          choices = handles$zeroloc)
      }
      else {
        showNotification(
          ui = "Value is too small...",
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
        (is.na(handles$poleloc)) || ((length(handles$poleloc) ==
                                      1) &&
                                     (handles$poleloc[1] == 0))) {
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
      if (length(handles$poleloc) == 1) {
        handles$poleloc <- NA
      }
      else {
        matchpoint <- match(tgt, handles$poleloc, nomatch = -1)
        str(matchpoint)
        temppoleloc <- NULL
        if ((matchpoint > 0) &&
            (matchpoint < length(handles$poleloc))) {
          temppoleloc <-
            c(handles$poleloc[(matchpoint + 1):length(handles$poleloc)])
        }
        if (matchpoint > 1) {
          temppoleloc <- c(handles$poleloc[1:(matchpoint - 1)],
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
        (is.na(handles$zeroloc)) || ((length(handles$zeroloc) ==
                                      1) &&
                                     (handles$zeroloc[1] == 0))) {
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
      if (length(handles$zeroloc) == 1) {
        handles$zeroloc <- NA
      }
      else {
        matchpoint <- match(tgt, handles$zeroloc, nomatch = -1)
        tempzeroloc <- NULL
        if ((matchpoint > 0) &&
            (matchpoint < length(handles$zeroloc))) {
          tempzeroloc <-
            c(handles$zeroloc[(matchpoint + 1):length(handles$zeroloc)])
        }
        if (matchpoint > 1) {
          tempzeroloc <- c(handles$zeroloc[1:(matchpoint - 1)],
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

# (c) &copy; 2016-2017 `ProductionMine`, Created by P. Squires, `DiYZed Research Group`, ECE Dept, University of Victoria?
