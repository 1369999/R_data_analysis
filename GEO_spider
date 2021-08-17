GEOdata<-function(input,count_perpage){
  ##########################black box
  require("rvest")
  require("httr")
  require("stringr")
  require("progress")
  url<-"https://www.ncbi.nlm.nih.gov/gds/"
  headers<-c("accept"="text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
             "accept-encoding"="gzip, deflate, br",
             "accept-language"="en-US,en;q=0.9,zh-CN;q=0.8,zh;q=0.7,en-GB;q=0.6",
             "content-type"="application/x-www-form-urlencoded",
             "user-agent"= "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.72 Safari/537.36 Edg/90.0.818.42)")
  cookies<-"ncbi_sid=0B87F728086AEA03_24515SID; pmc.article.report=; _ga=GA1.4.526528640.1619449401; __utma=194752562.526528640.1619449401.1621176102.1621176102.1; __utmc=194752562; __utmz=194752562.1621176102.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); _ce.s=v11.rlc~1622876626288; _ga=GA1.3.526528640.1619449401; QSI_HistorySession=https%3A%2F%[2Fwww.nlm.nih.gov](http://2Fwww.nlm.nih.gov)%2F~1622876632763%7Chttps%3A%2F%[2Fwww.nlm.nih.gov](http://2Fwww.nlm.nih.gov)%2Fbsd%2Funiform_requirements.html~1626075734311; _ga=GA1.2.526528640.1619449401; books.article.report=; _ga_7147EPK006=GS1.1.1626079577.3.1.1626079654.60; entrezSort=gds:; _gid=GA1.2.408168220.1628594707; _gat_ncbiSg=1; _gat_dap=1; WebEnv=1TawejAYv8RmBWz39Ozs6ZfwtmyikCI2TwAr9ftwng7Qx%400B87F728086AEA03_24515SID; ncbi_pinger=N4IgDgTgpgbg+mAFgSwCYgFwgMIFEAc+ADAOwAsAYgIxUBMArAMxm5VHscfkCcRVAgrQB0AWzhV6IADQgAxgBtksgNYA7KAA8ALplBFMIAGbJ5cGAEN5U4/K1QIcAOYQwU6+dlQt7z1riyAe1VvQKstAHcA/wD5aRAqAzgoYIgATwAVVLAoAHFUAGcfLyhUOHyocwhZRDjGbgM2bjIEmTJ9LDp8MnrWsgMFJTVNHVbJLH1WgDYDQ0tyuLISA0YJkDJ8A1pCBfqsOPp2kEQtLTB8jAB6C/CboVVZACNkO/kRO+REIUcAmAvHAouAH47BARABecwAVy0AWQIhEkPUAGJ9gk9jJ6LQDABlezIKD5EAAXxkiPkAXMqHU2l0IEYjBmcygtT6WC0EEhzJkjA26Lpu3iRG4JFWbQaWxIPTWrJAZIpVOGCzG8X20ywkyIkgxS3GcUmaJAJHwkhJcgC8KC1J0GFAWKwyXZUAAXrUDP9Ca0DKgArJ8pCRPsDHqvT6/QGZDqjloRLEZLyQLRVgLUFBZpDbHEaA1MwysLQeVr4jLypVqpm1SBLLH4pHZvJ5jIqPG6w2E4c8IRSJQaAxmKxOAOeHxBKJxIXaAaHdAnRh3Rhvb7/RgAHIAeWXuDitDtIBu4Tuj2eqle70+3xgW4FEiIfW5hzYRAZ3IN19vdJ33EmT7WuZARCEKwAfsv74Fs+wyvgGrEkSQA==="
  count=count_perpage
  page=1
  search<-input
  form<-list("term"=search,
             "EntrezSystem2.PEntrez.Gds.Gds_Facets.FacetsUrlFrag"="filters=;seriesGds",
             "EntrezSystem2.PEntrez.Gds.Gds_ResultsPanel.Gds_DisplayBar.sPageSize"= count,
             "EntrezSystem2.PEntrez.Gds.Gds_ResultsPanel.Gds_DisplayBar.sPageSize2"= count,
             "EntrezSystem2.PEntrez.Gds.Gds_ResultsPanel.Gds_DisplayBar.PageSize"= count,
             "EntrezSystem2.PEntrez.Gds.Gds_ResultsPanel.Gds_DisplayBar.LastPageSize"=count,
             "EntrezSystem2.PEntrez.Gds.Gds_ResultsPanel.Gds_DisplayBar.PrevPageSize"=count,
             "EntrezSystem2.PEntrez.Gds.Gds_ResultsPanel.Entrez_Pager.CurrPage"= page,
             "EntrezSystem2.PEntrez.Gds.Gds_Facets.FacetSubmitted"="true",
             "EntrezSystem2.PEntrez.Gds.Entrez_PageController.PreviousPageName"="results"
  )
  result<-POST(url,add_headers(.headers = headers),set_cookies(.cookies = cookies),body =form,encode = "form") 
  webpage<-read_html(result)
  summary_part<-webpage %>% html_nodes('div.supp') %>% html_text()
  Count<-webpage %>% html_nodes('h3.result_count.left') %>% html_text()
  GSE<-webpage %>% html_nodes(xpath='//dl[@class="rprtid"][1]/dd') %>% html_text()
  Title<-webpage %>% html_nodes('p.title a') %>% html_text()
  Sample_count<-webpage %>% html_nodes(xpath='//dl[@class="details"]/dt[2]') %>% html_text()
  Type<-webpage %>% html_nodes(xpath='//dl[@class="details lefty"][2]/dd') %>% html_text()
  Organism<-webpage %>% html_nodes(xpath='//dl[@class="details lefty"][1]/dd') %>% html_text()
  plat<-webpage %>% html_nodes(xpath='//dl[@class="details"]') %>% html_text(trim=F)
  hash="GPL\\d{1,6}"
  plat<-str_extract_all(plat,hash)
  plat<-as.character(plat)
  data<-data.frame(GSE=GSE,Title=Title,Sample_Count=Sample_count,Type=Type,
                   Organism=Organism,Platform=plat,summary_part=summary_part)
  aa<-data.frame(NULL)
  pb <- progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total =length(GSE), clear = FALSE, width= 60)##进度条
  #pb <- progress_bar$new(total = length(GSE))##进度条
  for (i in 1:length(GSE)) {
    url2<-paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",GSE[i])
    webpage<-read_html(url2)
    a<-webpage %>% html_nodes(xpath = '//tr[@valign="top"]') %>% html_text()
    b<-webpage %>% html_nodes(xpath = '//td[@style="text-align: justify"]') %>% html_text()
    Summary<-b[2]
    Design<-b[3]
    #Summary<-as.data.frame(a[grep(pattern = "^Summary",a)])
    #Design<-as.data.frame(a[grep(pattern = "^Overall",a)])
    Platform<-as.data.frame(a[grep(pattern = "^Platform",a)])
    aa[i,1]<-Summary
    aa[i,2]<-Design
    aa[i,3]<-Platform[1,1]
    rownames(aa)[i]<-GSE[i]
    colnames(aa)<-c("Summary","Design","Platform")
    pb$tick()
    Sys.sleep(1 / length(GSE))
  }
  output<-cbind(data,aa);return(output)}


GEOdata_simple<-function(input,count_perpage){
  ##########################black box
  require("rvest")
  require("httr")
  require("stringr")
  require("progress")
  url<-"https://www.ncbi.nlm.nih.gov/gds/"
  headers<-c("accept"="text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
             
             "accept-encoding"="gzip, deflate, br",
             
             "accept-language"="en-US,en;q=0.9,zh-CN;q=0.8,zh;q=0.7,en-GB;q=0.6",
             
             "content-type"="application/x-www-form-urlencoded",
             "user-agent"= "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.72 Safari/537.36 Edg/90.0.818.42)")
  cookies<-"ncbi_sid=0B87F728086AEA03_24515SID; pmc.article.report=; _ga=GA1.4.526528640.1619449401; __utma=194752562.526528640.1619449401.1621176102.1621176102.1; __utmc=194752562; __utmz=194752562.1621176102.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); _ce.s=v11.rlc~1622876626288; _ga=GA1.3.526528640.1619449401; QSI_HistorySession=https%3A%2F%[2Fwww.nlm.nih.gov](http://2Fwww.nlm.nih.gov)%2F~1622876632763%7Chttps%3A%2F%[2Fwww.nlm.nih.gov](http://2Fwww.nlm.nih.gov)%2Fbsd%2Funiform_requirements.html~1626075734311; _ga=GA1.2.526528640.1619449401; books.article.report=; _ga_7147EPK006=GS1.1.1626079577.3.1.1626079654.60; entrezSort=gds:; _gid=GA1.2.408168220.1628594707; _gat_ncbiSg=1; _gat_dap=1; WebEnv=1TawejAYv8RmBWz39Ozs6ZfwtmyikCI2TwAr9ftwng7Qx%400B87F728086AEA03_24515SID; ncbi_pinger=N4IgDgTgpgbg+mAFgSwCYgFwgMIFEAc+ADAOwAsAYgIxUBMArAMxm5VHscfkCcRVAgrQB0AWzhV6IADQgAxgBtksgNYA7KAA8ALplBFMIAGbJ5cGAEN5U4/K1QIcAOYQwU6+dlQt7z1riyAe1VvQKstAHcA/wD5aRAqAzgoYIgATwAVVLAoAHFUAGcfLyhUOHyocwhZRDjGbgM2bjIEmTJ9LDp8MnrWsgMFJTVNHVbJLH1WgDYDQ0tyuLISA0YJkDJ8A1pCBfqsOPp2kEQtLTB8jAB6C/CboVVZACNkO/kRO+REIUcAmAvHAouAH47BARABecwAVy0AWQIhEkPUAGJ9gk9jJ6LQDABlezIKD5EAAXxkiPkAXMqHU2l0IEYjBmcygtT6WC0EEhzJkjA26Lpu3iRG4JFWbQaWxIPTWrJAZIpVOGCzG8X20ywkyIkgxS3GcUmaJAJHwkhJcgC8KC1J0GFAWKwyXZUAAXrUDP9Ca0DKgArJ8pCRPsDHqvT6/QGZDqjloRLEZLyQLRVgLUFBZpDbHEaA1MwysLQeVr4jLypVqpm1SBLLH4pHZvJ5jIqPG6w2E4c8IRSJQaAxmKxOAOeHxBKJxIXaAaHdAnRh3Rhvb7/RgAHIAeWXuDitDtIBu4Tuj2eqle70+3xgW4FEiIfW5hzYRAZ3IN19vdJ33EmT7WuZARCEKwAfsv74Fs+wyvgGrEkSQA==="
  count=count_perpage
  page=1
  search<-input
  form<-list("term"=search,
             "EntrezSystem2.PEntrez.Gds.Gds_Facets.FacetsUrlFrag"="filters=;seriesGds",
             "EntrezSystem2.PEntrez.Gds.Gds_ResultsPanel.Gds_DisplayBar.sPageSize"= count,
             "EntrezSystem2.PEntrez.Gds.Gds_ResultsPanel.Gds_DisplayBar.sPageSize2"= count,
             "EntrezSystem2.PEntrez.Gds.Gds_ResultsPanel.Gds_DisplayBar.PageSize"= count,
             "EntrezSystem2.PEntrez.Gds.Gds_ResultsPanel.Gds_DisplayBar.LastPageSize"=count,
             "EntrezSystem2.PEntrez.Gds.Gds_ResultsPanel.Gds_DisplayBar.PrevPageSize"=count,
             "EntrezSystem2.PEntrez.Gds.Gds_ResultsPanel.Entrez_Pager.CurrPage"= page,
             "EntrezSystem2.PEntrez.Gds.Gds_Facets.FacetSubmitted"="true",
             "EntrezSystem2.PEntrez.Gds.Entrez_PageController.PreviousPageName"="results"
  )
  result<-POST(url,add_headers(.headers = headers),set_cookies(.cookies = cookies),body =form,encode = "form") 
  webpage<-read_html(result)
  Summary_part<-webpage %>% html_nodes('div.supp') %>% html_text()
  Count<-webpage %>% html_nodes('h3.result_count.left') %>% html_text()
  GSE<-webpage %>% html_nodes(xpath='//dl[@class="rprtid"][1]/dd') %>% html_text()
  Title<-webpage %>% html_nodes('p.title a') %>% html_text()
  Sample_count<-webpage %>% html_nodes(xpath='//dl[@class="details"]/dt[2]') %>% html_text()
  Type<-webpage %>% html_nodes(xpath='//dl[@class="details lefty"][2]/dd') %>% html_text()
  Organism<-webpage %>% html_nodes(xpath='//dl[@class="details lefty"][1]/dd') %>% html_text()
  plat<-webpage %>% html_nodes(xpath='//dl[@class="details"]') %>% html_text(trim=F)
  hash="GPL\\d{1,6}"
  plat<-str_extract_all(plat,hash)
  plat<-as.character(plat)
  data<-data.frame(GSE=GSE,Title=Title,Sample_Count=Sample_count,Type=Type,
                   Organism=Organism,Platform=plat,Summary_part=Summary_part)
  output<-data;return(output)}

inform<-"input and count per page are needed"


