#' Converting a Table into a Data Frame

X_tableToDf <- function(x){
  #x <- table(transitions)
  #names.tmp <- names(x)
  #df.tmp <- data.frame(matrix(NA))
  df.tmp <- data.frame(t(as.matrix(x)))
  rownames(df.tmp) <- NULL
  colnames(df.tmp) <- names.tmp
  return(df.tmp)

}

#' Preparing The Data Table for Making The Final Summary Table
#'
#' This function takes a table, a title, a subtitle, and a stud text, and puts it into the format required by X_create_final_table.
#'
#' @param x a table.
#' @param title.txt the table title (a character string).
#' @param subtitle.txt the table subtitle (a character string).
#' @param stud.txt a stud text (a character string).
#'
#' @export

X_prep_final_table <- function(x, title.txt, subtitle.txt, stub.txt){
#x <- table1.pre
#title.txt <- "Strain by Doe"
#subtitle.txt <- ""
#stub.txt <- ""
  title.blank <- subtitle.blank <- stubheader.blank <- rep(NA, ncol(x)+1)
  title.blank[1] <- paste0("TITLE: ", title.txt)
  subtitle.blank[1] <- paste0("SUBTITLE: ", subtitle.txt)
  stubheader.blank[1] <- paste0("STUBHEADER: ", stub.txt)
  row.1 <- c(NA,colnames(x))
  colnames(x) <- NULL
  final.table <- data.frame(matrix(NA, nrow=nrow(x)+4, ncol=ncol(x)+1))
  final.table[1,] <- title.blank
  final.table[2,] <- subtitle.blank
  final.table[3,] <- stubheader.blank
  final.table[4,] <- row.1
  final.table[5:nrow(final.table),2:ncol(final.table)] <- x

  #this is new:
  final.table[5:nrow(final.table),1] <- rownames(x)

  return(final.table)
}




#now I'm trying to create a more real one with spanner column labels and column labels
#dat.dir <- "/Users/jamesxenakis/Documents/learning/tables/output/output/"
#file.txt <- "test.xlsx"
#dat.raw <- as.data.frame(readxl::read_xlsx(paste0(dat.dir, file.txt), sheet=1, col_names=TRUE))

#' Function to Create the Final Summary Table
#'
#' @param x the table object.
#' @param sink.dir (what is this directory?)
#' @param table.dir the path to the directory that stores the produced table.
#' @param table.name the name of the table (a character string).
#'
#' @import gt
#' @export

X_create_final_table <- function(x, sink.dir, table.dir, table.name){
  # require(gt)

  #x=dat.raw
  #x=table1.pre
  #table.dir <- dat.dir
  #table.name <- table.name
  #x=table1.pre
  #table.dir=dat.dir
  #table.name="strainVsDose"
  #x <- dat.raw
  #table.dir <- "/Users/jamesxenakis/Documents/learning/tables/testfolder/"
  #table.name <- "xx"
  #x=final.table
  #table.dir=dat.dir
  #table.name="test"
  #x=tmp.table2.2
  #table.dir=dat.dir
  #table.name="inbredVsG0Summary"


  dat.raw <- x

  #print("1")

  #extract title, subtitle and stubheader
  title <- trimws(gsub("TITLE:", "", dat.raw[1,1]))
  #print(title)
  subtitle <- trimws(gsub("SUBTITLE:", "", dat.raw[2,1]))
  #print(subtitle)
  stubheader <- trimws(gsub("STUBHEADER:", "", dat.raw[3,1]))
  #print(stubheader)

  #grab the number of rows (remember the first column is title and row names)
  n.columns <- ncol(dat.raw)-1


  #print("2")
  #grab the actual data
  df <- dat.raw[4:nrow(dat.raw),]
  #grab the row names
  row.to.parse <- as.character(df[-1,1])
  #grab the column names
  col.to.parse <- as.character(df[1,-1])

#which columns have a pipe
#grep("\\|", col.to.parse)

#col.names are the first word before the pipe
col.names <- unlist(lapply(col.to.parse, function(x) strsplit(x, split="\\|")[[1]][1]))
#take the unique ones
unique.col.names <- unique(col.names)

#lowest.level.col.names are the second word after the pipe
lowest.level.col.names <- unlist(lapply(col.to.parse, function(x) strsplit(x, split="\\|")[[1]][2]))
lowest.level.col.names[which(is.na(lowest.level.col.names))] <- col.names[which(is.na(lowest.level.col.names))]

  #print("3")
#get the spanning columns
spanning.labels <- names(table(col.names))[which(table(col.names) > 1)]
spanned.col.ind <- list()

#only run this if there are spanning labels
if (length(spanning.labels) > 0){
for (j in 1:length(spanning.labels)){
  #j <-1
  current.feature <- grep(spanning.labels[j], col.names)
  spanned.col.ind <- append(spanned.col.ind, list(current.feature))
}
}

  #print("4")

#row.groups are the first word before the pipe
row.groups <- unlist(lapply(row.to.parse, function(x) strsplit(x, split="\\|")[[1]][1]))
unique.row.group <- unique(row.groups)


row.labels <- unlist(lapply(row.to.parse, function(x) strsplit(x, split="\\|")[[1]][2]))
row.labels[which(is.na(row.labels))] <- row.groups[which(is.na(row.labels))]


  #print("5")
#lowest.level.row.names <- unlist(lapply(col.to.parse, function(x) strsplit(x, split="\\|")[[1]][2]))
#lowest.level.col.names[which(is.na(lowest.level.col.names))] <- col.names[which(is.na(lowest.level.col.names))]

#spanned row.ind becomes a list of. THe elements of the list contain the indices of the rows that are spanned
spanned.row.ind <- list()
for (j in 1:length(unique.row.group)){
  #j <-1
  current.feature <- grep(unique.row.group[j], row.groups)
  spanned.row.ind <- append(spanned.row.ind, list(current.feature))
}

#So I will want to span rows 8 and 9 with the 8th
row.spanned.ind <- which(unlist(lapply(spanned.row.ind, FUN=length))>1)

  #print("6")
row.span.names <- unique.row.group[row.spanned.ind]
row.indices.to.span <- spanned.row.ind[row.spanned.ind]

#print(row.indices.to.span)
#print(row.indices.to.span[[1]])

#actual.dat <- df[min(which(!is.na(df[,1]))):nrow(df),]
#actual.dat  is the data with the row and column headings removed
actual.dat <- df[-1,-1]

#unique.row.groups <- unique(row.groups)

#print(length(row.labels))
#print(dim(actual.dat))

#print("7")
#I think I should change to all(row.groups == unique.row.group)
if(!is.na(unique.row.group)){
final.dat <- data.frame(row.labels, actual.dat)
} else {
#if no row labels!
final.dat <- actual.dat
}
#print(dim(final.dat))

#print("8")

#print(length(lowest.level.col.names))

if(!is.na(unique.row.group)){
colnames(final.dat) <- c("row_labels",lowest.level.col.names)
} else {
#colnames(final.dat) <- c("row_labels", "Column1","Col2a","Col2b","Column3")
colnames(final.dat) <- lowest.level.col.names
}

#print("9")


sink(paste0(sink.dir,"sinkcode.R"))
cat(
  "gt.object <- final.dat %>%
  gt(rowname_col = \"row_labels\") %>%
   tab_header(
    title = ")
cat(paste0("\"",title, "\",\n"))
cat("subtitle = ")
cat(paste0("\"",subtitle, "\"\n)"))

#only runt his chunk if there are spanning labels
if (length(spanning.labels) > 0){
for (i in 1:length(spanning.labels)){
  cat(paste0("%>% \n tab_spanner(
    label =\"",spanning.labels[i]),"\",columns=c(",paste0("`",paste0(lowest.level.col.names[spanned.col.ind[[i]]], collapse="`, `"),"`")  ,"))")
}
}

#run this conditionally
#if(!is.na(unique.row.group)){
if(length(row.indices.to.span) > 0){
for (i in 1:length(row.indices.to.span)){
  cat(paste0("%>% \n tab_row_group(
    label =\"",row.span.names[i]),"\",rows=c(",paste0(row.indices.to.span[[i]], collapse=", "),"))")
}
}
#tab_row_group(
#  label = "numbered",
#  rows = matches("^[0-9]")
#)
cat("\ngtsave(gt.object, \"")
cat(paste0(table.dir))
cat(paste0(table.name,".rtf"))
cat("\")")
cat("\ngtsave(gt.object, \"")
cat(paste0(table.dir))
cat(paste0(table.name,".tex"))
cat("\")")
sink()

#print("10")
#source(paste0(table.dir,"sinkcode.R"))
return(final.dat)
}
