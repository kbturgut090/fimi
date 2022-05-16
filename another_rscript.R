


system('git add -A')
system('git commit -m "all"')

system(paste('git push https://',Sys.getenv('GIT_ID'),'@github.com/kbturgut090/fimi.git',sep = ''))



