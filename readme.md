# Wins Created Basketball model

## Create a new season

1. mkdir data/ncaa/2016
   mkdir data/ncaa/2016/raw
   mkdir web/ncaa/2019

2. Run getmlb script:
   cd scripts
   ./ncaaget 2017

3. Update conferences (check https://en.wikipedia.org/wiki/NCAA_Division_I_conference_realignment)
   data/ncaa/conferences.txt

4. Test for errors in ncaa.R

5. Edit menu.html for new links

6. ftp -i wcstats.50webs.com

*****

5. cd ../web/basketball
   cp 2015_pos.html 2011_pos.html
   emacs 2011_pos.html
   change year references

5. emacs src/sidebar.html

6. cd ~/spt/web/bin
   lftp -f ncaab.src
