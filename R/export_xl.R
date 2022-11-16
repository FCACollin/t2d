#' Export Xl
#'
#' Wrapper around `openxlsx` to facilitate exports.
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#'
export_xl <- function(...,
                      info,
                      dir,
                      basenm,
                      rdata = FALSE) {

  df_ls <- list(...)
  wb <- openxlsx::createWorkbook()

  ws_nm_info <- "info"
  openxlsx::addWorksheet(wb, ws_nm_info)
  info_df <- c(info, Sys.info())
  info_df <- as.data.frame(info_df)
  openxlsx::writeData(
    wb = wb,
    sheet = ws_nm_info,
    x = info_df,
    rowNames = TRUE,
    colNames = FALSE
  )

  for (i in seq_along((df_ls))) {
    df <- df_ls[[i]]
    ws_nm <- names(df_ls)[i]
    lbl_df <- lapply(df, attr, "label")
    lbl_df <- lapply(lbl_df, \(x) if (is.null(x)) "" else x)
    lbl_df <- as.data.frame(lbl_df, row.names = "label:")
    openxlsx::addWorksheet(wb, ws_nm)
    openxlsx::writeData(
      wb = wb,
      sheet = ws_nm,
      x = lbl_df,
      rowNames = TRUE,
      colNames = FALSE
    )
    openxlsx::writeData(
      wb, ws_nm,
      x = df,
      rowNames = TRUE,
      startRow = 2
    )
  }

  if (rdata) {
    filenm <- file.path(dir, paste0(basenm, ".RData"))
    message("RData output (binary: ", filenm)
    info_ls <- setNames(info_df, nm = ws_nm_info)
    wcn <- c(info_ls, df_ls) # Weigh Correlation Network
    save(wcn, file = filenm)
  }

  filenm <- file.path(dir, paste0(basenm, ".xlsx"))
  message("Excel output: ", filenm)
  openxlsx::saveWorkbook(
    wb,
    file = filenm,
    overwrite = TRUE
  )
}
