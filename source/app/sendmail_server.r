### sendmail for tickets -> will also be sent as well as every error with additional background information
### no UI for this
### will be saved as sendmail_server.r

## Library needed: gmailr
# devtools::install_github("jimhester/gmailr")


####  send plain email

sendmail_car <- function(message, title, from, to, attach=NULL, type){

  # PROXY?  
  if(!is.null(config[["car.proxy"]]) || config[["car.proxy"]]=="")
  {
    setproxy <- TRUE
    
    options(RCurlOptions=list(proxy=config[["car.proxy"]], http.version=1))
    
    # Set proxy
    
    proxyhttrport <- config[["car.proxy.port"]]
    proxyhttr <- config[["car.proxy.url"]]
      #httr::use_proxy(url = proxyhttr, port=as.numeric(proxyhttrport))
    
   
  }
  
  # Get auth from JSON file
  if(identical(setproxy,TRUE))
  {
    httr::with_config(httr::use_proxy(url = proxyhttr, port=as.numeric(proxyhttrport), auth="basic"), gmailr::gmail_auth(secret_file = file.path(config[["wd"]],config[["gmailjson"]]), scope = 'compose'))
  }
  else {
    gmailr::gmail_auth(secret_file = file.path(config[["wd"]],config[["gmailjson"]]), scope = 'compose')  
  }
  
  
  # Get defaults
  if(is.null(from))
  {
    from <- config[["email.from"]]
  }
  if(is.null(to))
  {
    to <- config[["email.to"]]
  }
  if(is.null(message))
  {
    message <- "Log File Information"
  }
  if(is.null(type))
  {
    type <- "error"
  }
  
  # Prepare Email for error that occured
  if(type == "error")
  {
    if(!is.null(attach))
    {
      if(length(attach) == 1)
      {
        email <- gmailr::mime() %>%
        gmailr::to(to) %>%
        gmailr::from(from) %>%
        gmailr::subject(title) %>%
        gmailr::text_body(message) %>%
        gmailr::attach_file(attach)
      }
      else
      {
        email <- gmailr::mime() %>%
          gmailr::to(to) %>%
          gmailr::from(from) %>%
          gmailr::subject(title) %>%
          gmailr::text_body(message)
        print(attach)
        # Add all attachmants one by one
        for(i in 1:length(attach))
        {
          print(attach[i])
          email <- gmailr::attach_file(email, filename = attach[i])
        }
          
      }
      
    }
    else
    {
      email <- gmailr::mime() %>%
        gmailr::to(to) %>%
        gmailr::from(from) %>%
        gmailr::subject(title) %>%
        gmailr::text_body(message)
      
    }
    
 
    
  }
  
  # Send mail
  if(identical(setproxy,TRUE))
  {
    print("Send mail with proxy")
    httr::with_config(httr::use_proxy(url = proxyhttr, port=as.numeric(proxyhttrport), auth="basic"), gmailr::send_message(email, type = "multipart"))
  }
  else
  {
    print("Send mail no proxy")
    gmailr::send_message(email)  
  }
  
  
  return(TRUE)
}


# from <- config[["email.from"]] "you@account.com"
# to <- "sniper.de@gmail.com"
# title <- "[CRISPRAnalyzeR][Error]Email Subject"
# # could also be "[CRISPRAnalyzeR][log]....
# body <- "Email text, usually some log information"
# 
# # Make email draft
# 
# email <- mime() %>%
#   to("sniper.de@gmail.com") %>%
#   from("crispranalysis@gmail.com") %>%
#   subject(title) %>%
#   html_body(body) %>%
#   attach_file("config.r")
# send_message(email)
# 


