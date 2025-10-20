#' Gi en visuell fremstilling av registerets indikatorer over tid
#'
#' @param indikatordata En dataramme med følgende kolonner:
#'                 - ReshId
#'                 - year
#'                 - Teller
#'                 - Sykehusnavn
#'
#' @export
#'
ggPlotIndikator <- function(indikatordata, graaUt=NA, outfile = '',
                            lavDG=NA, inkl_konf=F, ant_aar = 3)
{
  indikator=indikatordata$indikator
  tittel=indikatordata$tittel
  terskel=indikatordata$terskel
  minstekrav=indikatordata$minstekrav
  maal=indikatordata$maal
  decreasing=indikatordata$decreasing
  maalretn=indikatordata$maalretn

  Tabell <- indikator |>
    dplyr::filter(year > max(year) - ant_aar) |>
    dplyr::summarise(Antall = sum(var),
                     N = sum(denominator),
                     .by = c(SykehusNavn, year)) |>
    dplyr::group_by(year) |>
    dplyr::group_modify(~ .x |> janitor::adorn_totals(name = "Nasjonalt")) |>
    dplyr::mutate(Andel = Antall/N*100) |>
    dplyr::mutate(Andel = ifelse(N < terskel, NA, Andel))

  Andel <- Tabell |>
    dplyr::select(SykehusNavn, year, Andel) |>
    dplyr::arrange(year) |>
    tidyr::pivot_wider(names_from = year,
                       values_from = c(Andel))
  N <- Tabell |>
    dplyr::select(SykehusNavn, year, N) |>
    dplyr::arrange(year) |>
    tidyr::pivot_wider(names_from = year,
                       values_from = c(N))

  Andel[N[[dim(N)[2]]] < terskel, -1] <- NA
  Andel_long <- Andel |>
    dplyr::arrange(
      desc(is.na(!!rlang::sym(dplyr::last(names(Andel))))),
      !!rlang::sym(dplyr::last(names(Andel)))
    ) |>
    dplyr::mutate(
      SykehusNavn = forcats::fct_inorder(SykehusNavn)
    ) |>
    tidyr::pivot_longer(cols = 2:dim(Andel)[2],
                        names_to = "year",
                        values_to = "andel") |>
    merge(Tabell |> dplyr::select(year, SykehusNavn, Antall, N),
          by = c("year", "SykehusNavn"), all.x = TRUE)


  aar <- sort(unique(Andel_long$year))
  library(ggplot2)
  library(plotly)
  library(dplyr)
  bakgrunn_gronn <- data.frame(
    xmin = 50,
    xmax = Inf,
    ymin = 0,
    ymax = dim(Andel)[1] + 1,
    color = "#A7EBCE"
  )
  bakgrunn_gul <- data.frame(
    xmin = 40,
    xmax = 50,
    ymin = 0,
    ymax = dim(Andel)[1] + 1,
    color = "#F5F0D5"
  )
  bakgrunn_rod <- data.frame(
    xmin = 0,
    xmax = 40,
    ymin = 0,
    ymax = dim(Andel)[1] + 1,
    color = "#F0DEDB"
  )
  alpha <- .8

  # x11()
  p <- ggplot(Andel_long,
              aes(x = andel, y = SykehusNavn, shape = year, color = year,
                  text = paste("Sykehus:", SykehusNavn, "<br>Antall:",
                               Antall, "<br>N:", N, "<br>Andel:", sprintf("%.1f", andel)))) +
    geom_rect(data = bakgrunn_gronn,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = bakgrunn_gronn$color,
              alpha = alpha, inherit.aes = FALSE) +
    geom_rect(data = bakgrunn_gul,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = bakgrunn_gul$color,
              alpha = alpha, inherit.aes = FALSE) +
    geom_rect(data = bakgrunn_rod,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = bakgrunn_rod$color,
              alpha = alpha, inherit.aes = FALSE) +
    geom_bar(data = Andel_long %>% filter(year == dplyr::last(aar)),
             aes(x = andel, y = SykehusNavn),
             stat = "identity", fill = "steelblue", alpha = 1, width = 4 / 5) +
    geom_point(size = 3) +
    scale_shape_manual(values = setNames(
      c(rev(c(19, 1, 17, 6, 15, 0)[1:(length(aar)-1)]), NA), aar)) +
    scale_color_manual(values = setNames(
      c(rep("black", length(aar)-1), "steelblue"), aar)) +
    scale_x_continuous(limits = c(0, 1.1*max(Andel_long$andel, na.rm = T)),
                       expand = c(0, 0)) +
    labs(title = tittel, x = "Andel (%)", y = element_blank()) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.title = element_text(vjust = 5, hjust = 0.5),
      plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")
    )

  plotly::ggplotly(p, tooltip = "text")


  #
  #
  #
  #   CType <- c("2022" = "black", "2023" = "black", "2024" = "steelblue")
  #   LType <- c("2022" = 19, "2023" = 1, "2024" = 24)
  #   x11()
  #   ggplot(Andel, aes(SykehusNavn, rad3)) +
  #     geom_col(width = 4 / 5, fill = "steelblue") +
  #     coord_flip() +
  #     scale_y_continuous(expand = c(0, 0)) +
  #     theme(
  #       panel.grid.major.y = element_blank(),
  #       panel.grid.minor.y = element_blank(),
  #       axis.ticks.y = element_blank(),
  #       panel.grid.major.x = element_blank(),
  #       panel.grid.minor.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       panel.background = element_rect(fill = "white"),
  #       plot.title = element_text(vjust = 5,
  #                                 hjust = 0.5),
  #       plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")
  #     ) +
  #     labs(title = tittel,
  #          y = "Andel (%)",
  #          x = element_blank()) +
  #     geom_point(aes(SykehusNavn, rad2), shape = 19) +
  #     geom_point(aes(SykehusNavn, rad1), shape = 1) +
  #     scale_color_manual(name = "legend", values = CType) +
  #     scale_shape_manual(name = "legend", values = LType) +
  #     theme(legend.position = "bottomright", legend.title = element_blank())
  #
  #   Andel <- tribble(
  #     ~SykehusNavn, ~rad1, ~rad2, ~rad3,
  #     "UNN-Harstad",  NA    , NA   , NA   ,
  #     "Kirkenes", NA    , NA   , NA   ,
  #     "Drammen",  0.386,  0   ,  1.29,
  #     "Stavern", 12.7  ,  5.34,  3.95,
  #     "NLSH", 34.0  , 33.3 , 14.3 ,
  #     "SI-Ottestad", NA    , 12.5 , 14.9 ,
  #     "St. Olavs", 21.6  , 16.1 , 17.3 ,
  #     "Levanger", NA    , 11.3 , 19.1 ,
  #     "Nasjonalt", 30.4  , 27.3 , 29.8 ,
  #     "UNN-Tromsø", 21.3  , 21.9 , 32.5 ,
  #     "Haukeland", 43.5  , 34.7 , 33.0 ,
  #     "Kristiansand", 31.9  , 34.0 , 34.2 ,
  #     "Sandnessjøen", 39.6  , 20.7 , 36.8 ,
  #     "OUS", 41.0  , 34.2 , 42.1 ,
  #     "Stord", NA    , NA   , 60.7 ,
  #     "Ålesund", 60.1  , 64.8 , 62.5 ,
  #     "Stavanger", 38.1  , 65.8 , 63.5
  #   )
  #
  #   CType1 <- c("2024" = "steelblue")
  #   CType <- c("2022" = "black", "2023" = "black")
  #   LType <- c("2022" = 1, "2023" = 19)
  #   x11()
  #   ggplot(Andel, aes(SykehusNavn, rad3, fill = "2024")) +
  #     geom_col(width = 4 / 5) +
  #     coord_flip() +
  #     scale_y_continuous(limits = c(0, 1.1*max(Andel[-1], na.rm = T)),
  #                        expand = c(0, 0)) +
  #     labs(title = tittel, y = "Andel (%)", x = element_blank()) +
  #     geom_point(aes(SykehusNavn, rad2, color = "2023", shape = "2023")) +
  #     geom_point(aes(SykehusNavn, rad1, color = "2022", shape = "2022")) +
  #     scale_color_manual(name = "legend", values = CType) +
  #     scale_shape_manual(name = "legend", values = LType) +
  #     scale_fill_manual(name = "legend", values = CType1) +
  #     theme(
  #       legend.position = "top",
  #       legend.title = element_blank(),
  #       panel.grid.major.y = element_blank(),
  #       panel.grid.minor.y = element_blank(),
  #       axis.ticks.y = element_blank(),
  #       panel.grid.major.x = element_blank(),
  #       panel.grid.minor.x = element_blank(),
  #       axis.ticks.x = element_blank(),
  #       panel.background = element_rect(fill = "white"),
  #       plot.title = element_text(vjust = 5, hjust = 0.5),
  #       plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")
  #     )














}

#' Gi en visuell fremstilling av registerets indikatorer over tid
#'
#' @param indikatordata En dataramme med følgende kolonner:
#'                 - ReshId
#'                 - year
#'                 - Teller
#'                 - Sykehusnavn
#'
#' @export
#'
nnrrPlotIndikator <- function(indikatordata, graaUt=NA, outfile = '',
                              lavDG=NA, inkl_konf=F)
{
  indikator=indikatordata$indikator
  tittel=indikatordata$tittel
  terskel=indikatordata$terskel
  minstekrav=indikatordata$minstekrav
  maal=indikatordata$maal
  skriftStr=indikatordata$skriftStr
  pktStr=indikatordata$pktStr
  legPlass=indikatordata$legPlass
  minstekravTxt=indikatordata$minstekravTxt
  maalTxt=indikatordata$maalTxt
  decreasing=indikatordata$decreasing
  width=indikatordata$width
  height=indikatordata$height
  maalretn=indikatordata$maalretn

  Tabell <- indikator |>
    dplyr::filter(year > max(year)-3) |>
    dplyr::summarise(Antall = sum(var),
                     N = dplyr::n(),
                     Andel = Antall/N*100,
                     .by = c(SykehusNavn, year))

  AntTilfeller <- tidyr::spread(Tabell[, -c(4,5)], 'year', 'Antall') %>%
    janitor::adorn_totals(name = "Nasjonalt")

  N <- tidyr::spread(Tabell[, -c(3,5)], 'year', 'N') %>%
    janitor::adorn_totals(name = "Nasjonalt")
  N[is.na(N)] <- 0

  # Andeler, inkludert nasjonalt
  andeler <- dplyr::bind_cols(AntTilfeller[,1], AntTilfeller[,-1]/N[,-1] * 100)

  # Fjern år med færre registreringer enn terskelverdi og sykehus med for lav dekningsgrad
  andeler[N < terskel] <- NA
  andeler[andeler$SykehusNavn %in% lavDG, -1] <- NA

  # Ordne rekkefølge, stigende eller synkende
  if (decreasing){
    rekkefolge <- order(andeler[, dim(andeler)[2]], decreasing = decreasing, na.last = F)
  } else {
    rekkefolge <- order(andeler[, dim(andeler)[2]], decreasing = decreasing, na.last = F)
  }

  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]

  # Skjul også tidligere år hvis siste år er sensurert pga. for få reg.
  # andeler[as.vector(N[, dim(andeler)[2]]<terskel), 2:3] <- NA
  andeler[as.vector(N[, dim(andeler)[2]]<terskel), 2:(dim(andeler)[2]-1)] <- NA


  # Beregn konfidensintervaller
  KI <- binomkonf(purrr::as_vector(AntTilfeller[rekkefolge, dim(andeler)[2]]),
                  purrr::as_vector(N[, dim(andeler)[2]]))*100
  KI[, is.na(andeler[, dim(andeler)[2]])] <- NA

  pst_txt <- paste0(sprintf('%.0f', purrr::as_vector(andeler[, dim(andeler)[2]])), ' %')
  # pst_txt[is.na(andeler[, dim(andeler)[2]])] <- paste0('N<', terskel, ' eller dekningsgrad mindre en 60 pst.')
  pst_txt[N[, dim(andeler)[2]]<terskel] <- paste0('N<', terskel)
  pst_txt[andeler$SykehusNavn %in% lavDG] <- 'Dekningsgrad < 60 %'
  pst_txt <- c(NA, pst_txt, NA, NA)

  FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], dim(andeler)[1])
  soyleFarger[which(andeler$SykehusNavn=='Nasjonalt')] <- farger[4]
  if (!is.na(graaUt[1])) {soyleFarger[which(andeler$SykehusNavn %in% graaUt)] <- 'gray88'}
  soyleFarger <- c(NA, soyleFarger)

  # Lagre parameterverdier
  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig
  oldpar_oma <- par()$oma

  cexgr <- skriftStr
  if (inkl_konf) {
    andeler$SykehusNavn <- paste0(andeler$SykehusNavn, ' (', purrr::as_vector(N[, dim(N)[2]]), ')')
    andeler <- rbind(andeler, c(NA,NA,NA))
    andeler$SykehusNavn[dim(andeler)[1]] <- paste0('(N, ', names(andeler)[dim(andeler)[2]], ')')
    KI <- cbind(c(NA, NA), KI, c(NA, NA))
  } else {
    andeler <- rbind(andeler, c(NA,NA,NA))
    andeler$SykehusNavn[dim(andeler)[1]] <- ''
  }

  andeler <- rbind(c(NA,NA), andeler, c(NA,NA))
  andeler$SykehusNavn[dim(andeler)[1]] <- ''
  andeler$SykehusNavn[1] <- ' '

  vmarg <- max(0, strwidth(andeler$SykehusNavn, units='figure', cex=cexgr)*0.75)
  # par('fig'=c(vmarg, 1, 0, 1))
  # x11()
  par('mar'=c(5.1, 8.1, 5.1, 9.1))
  # par('oma'=c(0,1,0,0))

  if (inkl_konf) {
    par('mar'=c(5.1, 4.1, 5.1, 2.1))
    xmax <- min(max(KI, na.rm = T)*1.15,100)
  } else {
    xmax <- min(100, 1.15*max(andeler[,-1], na.rm = T))
  }

  ypos <- barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = 'Andel (%)')

  fargerMaalNiva <-  c('aquamarine3','#fbf850', 'red')

  if (maal > minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=minstekrav, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[2], border = NA)
    rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (maal < minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=maal, ybottom=1, xright=minstekrav, ytop=max(ypos)-1.6, col = fargerMaalNiva[2], border = NA)
    rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='lav') {
    # rect(xleft=maal, ybottom=0, xright=minstekrav, ytop=max(ypos)+0.4, col = fargerMaalNiva[2], border = NA)
    rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='hoy') {
    # rect(xleft=maal, ybottom=0, xright=minstekrav, ytop=max(ypos)+0.4, col = fargerMaalNiva[2], border = NA)
    rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}

  barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
           names.arg=rep('',dim(andeler)[1]),
           horiz=T, axes=F, space=c(0,0.3),
           col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)

  title(main = tittel)
  ypos <- as.numeric(ypos) #as.vector(ypos)
  yposOver <- max(ypos)-2 + 0.5*diff(ypos)[1]
  if (!is.na(minstekrav)) {
    lines(x=rep(minstekrav, 2), y=c(-1, yposOver), col=fargerMaalNiva[2], lwd=2)
    par(xpd=TRUE)
    text(x=minstekrav, y=yposOver, labels = minstekravTxt,
         pos = 4, cex=cexgr*0.65, srt = 90)
    par(xpd=FALSE)
  }
  if (!is.na(maal)) {
    lines(x=rep(maal, 2), y=c(-1, yposOver), col=fargerMaalNiva[1], lwd=2)
    barplot( t(andeler[, dim(andeler)[2]]), beside=T, las=1,
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
    par(xpd=TRUE)
    text(x=maal, y=yposOver, labels = maalTxt, pos = 4, cex=cexgr*0.65, srt = 90) #paste0(maalTxt,maal,'%')
    par(xpd=FALSE)
  }
  if (inkl_konf){
    arrows(x0 = KI[1,], y0 = ypos, x1 = KI[2,], y1 = ypos,
           length=0.5/max(ypos), code=3, angle=90, lwd=1.8, col='gray') #, col=farger[1])
    legend('bottom', cex=0.9*cexgr, bty='n',
           lwd=1.8, lty = 1, pt.cex=1.8, col='gray',
           legend=paste0('Konfidensintervall ', names(N)[dim(N)[2]]))
  }

  axis(1,cex.axis=0.9)
  mtext( andeler$SykehusNavn, side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  antAar <- dim(andeler)[2]-1

  if (!inkl_konf) {
    if (dim(N)[2] == 4) {
      mtext( c(NA, purrr::as_vector(N[,2]), names(N)[2], NA, NA), side=4,
             line=2.5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
      mtext( c(NA, purrr::as_vector(N[,3]), names(N)[3], NA, NA), side=4,
             line=5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
      mtext( c(NA, purrr::as_vector(N[,4]), names(N)[4], NA, NA), side=4,
             line=7.5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
      mtext( 'N', side=4, line=5.0, las=1, at=max(ypos), col=1, cex=cexgr*.7, adj = 1)
    }
    if (dim(N)[2] == 3) {
      mtext( c(NA, purrr::as_vector(N[,2]), names(N)[2], NA, NA), side=4,
             line=3, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
      mtext( c(NA, purrr::as_vector(N[,3]), names(N)[3], NA, NA), side=4,
             line=6, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
      mtext( 'N', side=4, line=4.0, las=1, at=max(ypos), col=1, cex=cexgr*.7, adj = 1)
    }

  }

  par(xpd=TRUE)
  if (dim(N)[2] == 4) {
    points(y=ypos, x=purrr::as_vector(andeler[,2]),cex=pktStr) #'#4D4D4D'
    points(y=ypos, x=purrr::as_vector(andeler[,3]),cex=pktStr,pch= 19)
    par(xpd=FALSE)
    if (legPlass=='nede'){
      legend(x=82, y=ypos[2]+1 ,xjust=0, cex=cexgr, bty='n', #bg='white', box.col='white',
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
             legend=names(N) )}
    if (legPlass=='top'){
      legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
             legend=names(N[,-1]), ncol = dim(andeler)[2]-1)
    }
  }


  text(x=0, y=ypos, labels = pst_txt, cex=0.75, pos=4)#
  if ( outfile != '') {dev.off()}

}
