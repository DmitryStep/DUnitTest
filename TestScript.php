<?php
// --------------------------------------- Набор CURL - функций для обмена данными с сервером ------------------------------------------------

// Создаём соединение с уже пройденной авторизацией (нужный кукиш копируется в www/tmp перед стартом скрипта)
function create_Curl_Connect($Referer_URL){
  $Result = curl_init();
  curl_setopt($Result, CURLOPT_VERBOSE, True);
  curl_setopt($Result, CURLOPT_RETURNTRANSFER, False);
  curl_setopt($Result, CURLOPT_UNRESTRICTED_AUTH, True);
  curl_setopt($Result, CURLOPT_HEADER, False);
  curl_setopt($Result, CURLOPT_SSL_VERIFYPEER, false);
  curl_setopt($Result, CURLOPT_SSL_VERIFYHOST, false);
  curl_setopt($Result, CURLOPT_VERBOSE, True);
  curl_setopt($Result, CURLOPT_RETURNTRANSFER, False);
  curl_setopt($Result, CURLOPT_UNRESTRICTED_AUTH, True);
  curl_setopt($Result, CURLOPT_REFERER, $Referer_URL);
  return $Result;
}

// Посылаем запрос, на выходе получаем текст JSON
function SendRequest($Curl_Var, $Request_Str){
  curl_setopt($Curl_Var, CURLOPT_URL, $Request_Str);
  curl_setopt($Curl_Var, CURLOPT_COOKIE, "PHPSESSID=2m7ee1t6374qpol7bk3e1geb11"); // <-- а это тот самый кукишок с уже готовой авторизацией внутри
  $Result = curl_exec($Curl_Var);
  return $Result;
}

// Убийца соединения
function close_Curl_Connect($Curl_Var){
  curl_close($Curl_Var);
}

// ------------------------------------------------------ Конец набора функций CURL ------------------------------------------------------------


// Forming of Request

function GetRequest($URL, $File_string){
	$Params = explode("#", $File_string);
	$Result = "";
	if (count($Params) > 1){
		$ValueIndex = 1;
		$Request = explode("&", $Params[0]);
		foreach ($Request as $Request_param){
			$Result = $Result.$Request_param.$Params[$ValueIndex]."&";
			$ValueIndex++;
		}
		$Result = rtrim($Result, "&");
	}
	else{
		$Result = $Params[0];
	}
	$Result = $URL.$Result;
	return $Result;
}

// Read from file
function readtestfile($input_file){
  $tests_array = "";
  $testfile = fopen($input_file, "r");
  $Index = 0;
  while (!feof($testfile)){
    $tests_array[$Index] = fgets($testfile);
    $Index++;
  }
  fclose($testfile);
  return $tests_array;
}

// Parse JSON
function ParseJSONResponce($Response){
  $Result = JSON_decode($Response);
  return $Result;
}


// Assert test results
function AssertResults(){
}

// Output to log in NUnit-format
function OutputToXMLLog($output_file){
}

// ---------------------------- End of functions -------------------------------

// ---------------------------- Main Program -----------------------------------

$URL_str = "http://test.ils-glonass.ru";

$tests = readtestfile("tests2.txt");

$ch = create_Curl_Connect($URL_str);
foreach ($tests as $test) {
  $Request = GetRequest($URL_str, $test);
  echo $Request."<br>";
  $Response = SendRequest($ch, $Request);
//  echo "--------------------------------------------------------------\n";
  echo $Response."<br>";
}

close_Curl_Connect($ch);

?>
