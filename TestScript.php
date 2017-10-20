<?php

// Read file
$testfile = fopen("tests.txt", "r");

while (!feof($testfile)){
  echo fgets($testfile)."<br>";
}

fclose($testfile);


?>
