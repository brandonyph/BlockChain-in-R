library(openssl)

setClass("link",representation(timestamp="POSIXct",
                                account_from="numeric",
                                account_to="numeric",
                                transaction="numeric",
                                block_no = "numeric",
                                hash_no = "character"
                                ))



S1 <- new("link",
          timestamp=Sys.time(),
          account_from=11224, 
          account_to = 45121,
          transaction=0.225,
          block_no=1,
          hash_no = as.character(sha256("TheFirstHashIsNotImportant")))


S2 <- new("link",
          timestamp=Sys.time(),
          account_from=74581, 
          account_to = 96512,
          transaction=1.526,
          block_no=2,
          hash_no = as.character(sha256("ThisShouldBeCalculatedByIamLazy")))

Chain <- c(S1,S2)

createtransction <- function(timestamp=Sys.time(),
                         account_from=0, 
                         account_to=0,
                         transaction=0,
                         chain=Chain
                         ){
                      
                                    previous_hash <-  Chain[[length(Chain)]]@hash_no
                                    block_no =Chain[[length(Chain)]]@block_no+1
                                    hash_no <- as.character(sha256(paste(as.character(timestamp),
                                                                         as.character(account_from),
                                                                         as.character(account_to),
                                                                         as.character(transaction),
                                                                         as.character(block_no),
                                                                         as.character(previous_hash))))
                                    out <- new("link",timestamp=timestamp,account_from=account_from,account_to=account_to,transaction=transaction,block_no=block_no,hash_no=hash_no)
                                    Chain  <- c(Chain,out) 
                                    return(Chain)
                                    }


Chain<- createtransction(account_from=sample(1:99999,1), 
                 account_to=sample(1:99999,1),
                 transaction=runif(n=1, min=1e-3,max=.99))


