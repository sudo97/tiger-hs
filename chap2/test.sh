for file in `ls ../testcases/`
do
  echo "/*<<<<< $file >>>>>*/"
  echo
  stack exec chap2-exe -- "../testcases/$file"
  echo "/* >>>>> end of $file <<<<< */"
done