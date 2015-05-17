test_host=ya.ru


function test_connection {
  meta Testing connection
  output=$(ping -w 5 -c 1 -q $test_host)
  code=$?
  sed 2,3d <<< $output
  if [[ $code -eq 0 ]] {
    meta Done
  }
}


function meta { # Служебная функция для распечатки метаинформации
  echo
  tput bold
  echo $*
  tput sgr0
}
