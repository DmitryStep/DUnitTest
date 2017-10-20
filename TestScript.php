<?php

$testfile = fopen("e:\\dev_php\\tests.xml", "r");

while (!feof($testfile)){
  echo fgets($testfile);
}

fclose($testfile);


?>