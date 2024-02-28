#' Gjennomsnitt av valgt variabel pr grupperingsvariabel
#'
#' Denne funksjonen beregner gjennomsnitt for valgt variabel og plotter de per grupperingsvariabel (vanligvis sykehus) for
#' de siste trre år
#'
#' Her kan detaljer skrives
#'
#' @inheritParams nnrrFigAndeler
#'
#' @return En figur med gjennomsnitt av valgt variabel pr grupperingsvariabel
#'
#' @export
#'
nnrrFigGjsnGrVarTid <- function(RegData, valgtVar='PatientAge', tittel='', datoFra='2014-01-01', datoTil='2050-12-31',
                                minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                                minald=0, maxald=130, erMann=99, outfile='', gr_var='SykehusNavn', terskel=30,
                                lavDG="", decreasing=F, legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA,
                                width=800, height=700, inkl_konf=F, maalretn='hoy')

{

  RegData<-RegDataAll; valgtVar='PatientAge'; tittel='Gjennomsnittsalder'; datoFra='2014-01-01'; datoTil='2019-12-31';
  minstekrav = NA; maal = NA; skriftStr=1.3; pktStr=1.4;
  minald=0; maxald=130; erMann=99; outfile=''; gr_var='SykehusNavn'; terskel=30;
  lavDG=""; decreasing=F; legPlass='top'; minstekravTxt='Min.'; maalTxt='Mål'; graaUt=NA;
  width=800; height=700; inkl_konf=F; maalretn='hoy'

  RegData$Gr_var <- RegData[, gr_var]

  RegData$Variabel <- RegData[, valgtVar]

  RegData <- RegData[!is.na(RegData$Variabel), ]

  ## Gjør utvalg basert på brukervalg (nnrrUtvalg)
  NNRRUtvalg <- nnrrUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald,
                           maxald=maxald, erMann=erMann)
  RegData <- NNRRUtvalg$RegData
  utvalgTxt <- NNRRUtvalg$utvalgTxt

  RegData <- RegData[which(RegData$Aar > max(RegData$Aar)-3), ] # behold bare siste 3 år

  Tabell <- RegData %>% dplyr::group_by(Gr_var, Aar) %>%
    dplyr::summarise(gj.sn = mean(Variabel),
                     std.avvik= sd(Variabel),
                     N = dplyr::n())
  aux <- RegData %>% dplyr::group_by(Aar) %>%
    dplyr::summarise(gj.sn = mean(Variabel),
                     std.avvik= sd(Variabel),
                     N = dplyr::n())
  aux$Gr_var <- 'Samlet'
  aux <- aux[, c(5,1:4)]
  Tabell <- dplyr::bind_rows(Tabell, aux)


  gj.sn <- tidyr::spread(Tabell[, -c(4,5)], 'Aar', 'gj.sn')
  N <- tidyr::spread(Tabell[, -c(3,4)], 'Aar', 'N')
  SD <- tidyr::spread(Tabell[, -c(3,5)], 'Aar', 'std.avvik')


  # Fjern år med færre registreringer enn terskelverdi og sykehus med for lav dekningsgrad
  gj.sn[N < terskel] <- NA
  gj.sn[gj.sn$Gr_var %in% lavDG, -1] <- NA

  # Ordne rekkefølge, stigende eller synkende
  # if (decreasing){
    rekkefolge <- order(gj.sn[[dim(gj.sn)[2]]], decreasing = decreasing, na.last = F)
  # } else {
  #   rekkefolge <- order(gj.sn[[dim(gj.sn)[2]]], decreasing = decreasing, na.last = F)
  # }

  gj.sn <- gj.sn[rekkefolge, ]
  N <- N[rekkefolge, ]

  # Skjul også tidligere år hvis siste år er sensurert pga. for få reg.
  gj.sn[as.vector(N[, dim(gj.sn)[2]]<terskel), 2:3] <- NA

  # Beregn konfidensintervaller
  KI <- t(matrix(c(purrr::as_vector(gj.sn[, dim(gj.sn)[2]]) -
             qt(.975, purrr::as_vector(N[, dim(N)[2]]) - 1)*purrr::as_vector(SD[, dim(N)[2]])/sqrt(purrr::as_vector(N[, dim(N)[2]])),
           purrr::as_vector(gj.sn[, dim(gj.sn)[2]]) +
             qt(.975, purrr::as_vector(N[, dim(N)[2]]) - 1)*purrr::as_vector(SD[, dim(N)[2]])/sqrt(purrr::as_vector(N[, dim(N)[2]]))),
         ncol = 2))


  KI[, is.na(gj.sn[, dim(gj.sn)[2]])] <- NA
  KI <- cbind(c(NA,NA), KI, c(NA,NA), c(NA,NA))

  pst_txt <- paste0(sprintf('%.1f', purrr::as_vector(gj.sn[, dim(gj.sn)[2]])))
  # pst_txt[is.na(gj.sn[, dim(gj.sn)[2]])] <- paste0('N<', terskel, ' eller dekningsgrad mindre en 60 pst.')
  pst_txt[N[, dim(gj.sn)[2]]<terskel] <- paste0('N<', terskel)
  pst_txt[gj.sn$Gr_var %in% lavDG] <- 'Dekningsgrad < 60 %'
  pst_txt <- c(NA, pst_txt, NA, NA)


  FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], dim(gj.sn)[1])
  soyleFarger[which(gj.sn$Gr_var=='Samlet')] <- farger[4]
  if (!is.na(graaUt[1])) {soyleFarger[which(gj.sn$Gr_var %in% graaUt)] <- 'gray88'}
  soyleFarger <- c(NA, soyleFarger, NA, NA)

  # Lagre parameterverdier
  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig
  oldpar_oma <- par()$oma

  cexgr <- skriftStr
  # if (inkl_konf) {
  #   gj.sn$Gr_var <- paste0(gj.sn$Gr_var, ' (', purrr::as_vector(N[, dim(N)[2]]), ')')
  #   gj.sn <- rbind(as.data.frame(gj.sn), c(paste0('(N, ', names(gj.sn)[dim(gj.sn)[2]], ')'), NA,NA,NA))
  #   # gj.sn$Gr_var[dim(gj.sn)[1]] <- paste0('(N, ', names(gj.sn)[dim(gj.sn)[2]], ')')
  #   KI <- cbind(c(NA, NA), KI, c(NA, NA))
  # } else {
  #   gj.sn <- rbind(gj.sn, c(NA,NA,NA))
  #   gj.sn$Gr_var[dim(gj.sn)[1]] <- ''
  # }
  #
  # gj.sn <- rbind(c(NA,NA), gj.sn, c(NA,NA))
  # gj.sn$Gr_var[dim(gj.sn)[1]] <- ''
  # gj.sn$Gr_var[1] <- ' '

  gj.sn <- rbind(c('',NA,NA,NA), as.data.frame(gj.sn), c('',NA,NA,NA), c('',NA,NA,NA))
  gj.sn[,-1] <- apply(gj.sn[,-1], 2, as.numeric)

  vmarg <- max(0, strwidth(gj.sn$Gr_var, units='figure', cex=cexgr)*0.75)
  # par('fig'=c(vmarg, 1, 0, 1))
  # x11()
  par('mar'=c(5.1, 8.1, 5.1, 9.1))
  # par('oma'=c(0,1,0,0))

  if (inkl_konf) {
    # par('mar'=c(5.1, 4.1, 5.1, 2.1))
    xmax <- max(KI, na.rm = T)*1.15
  } else {
    xmax <- 1.15*max(gj.sn[,-1], na.rm = T)
  }

  ypos <- barplot( t(gj.sn[,dim(gj.sn)[2]]), beside=T, las=1,
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(gj.sn)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = 'Gjennomsnittsverdi')

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

  barplot( t(gj.sn[,dim(gj.sn)[2]]), beside=T, las=1,
           names.arg=rep('',dim(gj.sn)[1]),
           horiz=T, axes=F, space=c(0,0.3),
           col=soyleFarger, border=NA, xlab = 'Gjennomsnittsverdi', add=TRUE)

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
    barplot( t(gj.sn[, dim(gj.sn)[2]]), beside=T, las=1,
             names.arg=rep('',dim(gj.sn)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Gjennomsnitt', add=TRUE)
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
  mtext( gj.sn$Gr_var, side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  antAar <- dim(gj.sn)[2]-1

  # if (!inkl_konf) {
    mtext( c(NA, purrr::as_vector(N[,2]), names(N)[2], NA, NA), side=4, line=2.5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
    mtext( c(NA, purrr::as_vector(N[,3]), names(N)[3], NA, NA), side=4, line=5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
    mtext( c(NA, purrr::as_vector(N[,4]), names(N)[4], NA, NA), side=4, line=7.5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
    mtext( 'N', side=4, line=5.0, las=1, at=max(ypos), col=1, cex=cexgr*.7, adj = 1)
  # }
  # else {
  #   mtext( '(N)', side=2, line=0.3, las=1, at=max(ypos)+diff(ypos)[1], col=1, cex=cexgr, adj = 1)
  # }

  par(xpd=TRUE)
  points(y=ypos, x=purrr::as_vector(gj.sn[,2]),cex=pktStr) #'#4D4D4D'
  points(y=ypos, x=purrr::as_vector(gj.sn[,3]),cex=pktStr,pch= 19)
  par(xpd=FALSE)
  if (legPlass=='nede'){
    legend(x=82, y=ypos[2]+1 ,xjust=0, cex=cexgr, bty='n', #bg='white', box.col='white',
           lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
           legend=names(N) )}
  if (legPlass=='top'){
    legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
           lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
           legend=names(N[,-1]), ncol = dim(gj.sn)[2]-1)
  }
  # }

  text(x=0, y=ypos, labels = pst_txt, cex=0.75, pos=4)#
  if ( outfile != '') {dev.off()}
















}

