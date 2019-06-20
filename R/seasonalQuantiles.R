seasonalQuantiles<-function(variable = "LFMC", SiteCode = 1, SpeciesCode = 1, daywidth = 15,
                            probs = c(0.1, 0.25,0.5,0.75, 0.9), na.rm= T, draw = TRUE,
                            splines = TRUE, drawCurrentYear = TRUE) {
  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), get("lfmcdbfile"))
  rs <- dbSendQuery(lfmc_db, paste0('SELECT * FROM lfmc WHERE "SiteCode" = :x AND "SpeciesCode" = :y'),
                    params = list(x = SiteCode, y=SpeciesCode))
  df = dbFetch(rs, n = -1)
  dbClearResult(rs)
  dbDisconnect(lfmc_db)
  cat(paste0("Number of records: ", nrow(df) ,"\n"))
  df$Date = as.Date(df$Date, origin = "1970-01-01")
  df$DOY = as.numeric(strftime(df$Date, format = "%j"))
  df$Year = as.numeric(strftime(df$Date, format = "%Y"))
  seqDOY = seq(0,366, by = daywidth)
  midPoints = 0.5*(seqDOY[1:(length(seqDOY)-1)]+seqDOY[2:length(seqDOY)])
  df$DOYclass = cut(df$DOY, seqDOY)
  l = levels(df$DOYclass)
  nclasses = length(l)
  currentYear = max(df$Year)
  doy_current = df$DOY[df$Year==currentYear]
  v_current = df[[variable]][df$Year==currentYear]
  x = matrix(NA, nrow = nclasses, ncol = length(probs))
  rownames(x) = levels(df$DOYclass)
  colnames(x) = probs
  for(i in 1:nclasses) {
    v = df[[variable]][df$DOYclass==l[i]]
    x[i,] = quantile(v, probs = probs, na.rm=na.rm)
  }
  if(!draw) {
    return(x)
  } else {
    xs = numeric(0)
    ys = numeric(0)
    q = numeric(0)
    rownames(x)
    for(i in 1:ncol(x)) {
      if(splines) {
        d = data.frame(x =midPoints, y = x[,i])
        s = spline(d, n = nrow(x)*3)
        xs = c(xs, s$x)
        ys = c(ys, s$y)
        q = c(q, rep(100*as.numeric(colnames(x)[i]),length(s$x)))
      } else {
        xs = c(xs, midPoints)
        ys = c(ys, x[,i])
        q = c(q, rep(100*as.numeric(colnames(x)[i]),length(x[,i])))
      }
    }
    df = data.frame(x = xs, y = ys,
                    quantile = factor(paste0(q, "%"),
                                      levels = paste0(as.numeric(colnames(x))*100, "%")))
    g<-ggplot(df, aes(x=x, y=y))+
      geom_line(aes(col=quantile), size = 1)+
      xlab("DOY")+ ylab(variable)
    if(drawCurrentYear) {
      df2 = data.frame(x = doy_current, y= v_current)
      g <- g+
        geom_point(aes(x=x, y=y),data = df2)
    }
    return(g)
  }
}


seasonalProbability<-function(variable = "LFMC", targetYear = 2019,
                              SiteCode = 1, SpeciesCode = 1, daywidth = 15) {
  lfmc_db <- DBI::dbConnect(RSQLite::SQLite(), get("lfmcdbfile"))
  rs <- dbSendQuery(lfmc_db, paste0('SELECT * FROM lfmc WHERE "SiteCode" = :x AND "SpeciesCode" = :y'),
                    params = list(x = SiteCode, y=SpeciesCode))
  df = dbFetch(rs, n = -1)
  dbClearResult(rs)
  dbDisconnect(lfmc_db)
  cat(paste0("Number of records: ", nrow(df) ,"\n"))
  df$Date = as.Date(df$Date, origin = "1970-01-01")
  df$DOY = as.numeric(strftime(df$Date, format = "%j"))
  df$Year = as.numeric(format(df$Date, format = "%Y"))
  seqDOY = seq(0,366, by = daywidth)
  midPoints = 0.5*(seqDOY[1:(length(seqDOY)-1)]+seqDOY[2:length(seqDOY)])
  df$DOYclass = cut(df$DOY, seqDOY)
  l = levels(df$DOYclass)
  nclasses = length(l)
  m = rep(NA, nrow = nclasses)
  n = rep(NA, nrow = nclasses)
  p = rep(NA, nrow = nclasses)
  for(i in 1:nclasses) {
    v = df[[variable]][df$DOYclass==l[i]]
    y = df$Year[df$DOYclass==l[i]]
    m[i] = mean(v[y==targetYear], na.rm=T)
    n[i] = length(v)
    p[i] = sum(v<=m[i], na.rm=T)/n[i]
    if(is.na(m[i])) {
      m[i] = NA
      p[i] = NA
    }
  }
  return(data.frame(n = n, val = m, prob = p, row.names = l))
}
