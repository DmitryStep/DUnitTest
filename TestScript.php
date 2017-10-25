<?php

// -------------- Набор функций для обмена данными с сервером --------------

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

// ---------------------- Конец набора функций CURL -----------------


// Forming of Request
function GetRequest($URL, $Request_str, $Parameters_str){
	$Params = explode("#", $Parameters_str);
	$Result = "";
	if (count($Params) > 1){
		$ValueIndex = 0;
		$Request = explode("&", $Request_str);
		foreach ($Request as $Request_param){
			$Result = $Result.$Request_param.$Params[$ValueIndex]."&";
			$ValueIndex++;
		}
		$Result = rtrim($Result, "&");
	}
	else{
		$Result = $Request_str;
	}
	$Result = $URL.$Result;
	return $Result;
}

// Read from file
function readtestfile($input_file){
  $Result = file_get_contents($input_file);
  return $Result;
}

// Parse JSON
function ParseJSONTests($JSONdata){
  $Result = JSON_decode($JSONdata);
  return $Result;
}

function ParseJSONResponse($Response){
  $Result = JSON_decode($Response);
  return $Result;
}

function Fail($TestCase){
  $Result->testresult = 'failed';
  $Result->errorStackTrace = $TestCase->failmessage;
  return $Result;
}

function Pass($TestCase){
  $Result->testresult = 'passed';
  $Result->errorStackTrace = "";
  return $Result;
}

function Error($TestCase, $error){
  $Result->testresult = "error";
  $Result->errorStackTrace = $error;
  return $Result;
}

// Assert test results
function AssertTest($TestCase){
  try{
//    $ActualResult = $TestCase->$ExpectedField;
    $Operation = $TestCase->operation;
    $ExpectedResult = $TestCase->expectedresult;
    if ($Operation == "="){
      $AssertionResult = ($ExpectedResult == $ActualResult);
    }
    if ($Operation == "!="){
      $AssertionResult = ($ExpectedResult != $ActualResult);
    }
    if ($Operation == ">="){
      $AssertionResult = ($ExpectedResult >= $ActualResult);
    }
    if ($Operation == "<="){
      $AssertionResult = ($ExpectedResult <= $ActualResult);
    }
    if ($Operation == ">"){
      $AssertionResult = ($ExpectedResult > $ActualResult);
    }
    if ($Operation == "<"){
      $AssertionResult = ($ExpectedResult < $ActualResult);
    }

    if ($AssertionResults==false){
      $Result = Fail($TestCase);
    }
    else{
      $Result = Pass($TestCase);
    }
  }catch(Exception $e){
    $Result = Error($TestCase, $e->$Message);
  }
  return $Result;
}

// Output to log in JUnit-format
function OutputToXMLLog($output_file, $TestResultsArray, $FullDuration){
  $XML_doc = new DomDocument('1.0');
//  <result plugin="junit@1.21">
  $TestReport = $XML_doc->appendChild($XML_doc->createElement('result'));
  $Suite = $TestReport->appendChild($XML_doc->createElement('suites'))->appendChild($XML_doc->createElement('suite'));
  $Suite->appendChild($XML_doc->createTextNode($TestResultsArray->suitename));
  $TestCases = $Suite->appendChild($XML_doc->createElement('cases'));
  foreach ($TestResultsArray as $TestResult){
    $TestCase = $TestCases->appendChild($XML_doc->createElement('case'));
    $Duration = $TestCase->appendChild($XML_doc->createElement('duration'));
    $Duration->appendChild($XML_doc->createTextNode($TestResultsArray->duration));
    $TestName = $TestCase->appendChild($XML_doc->createElement('testName'));
    $TestName->appendChild($XML_doc->createTextNode($TestResultsArray->testname));
    $ErrorStackTrace = $TestCase->appendChild($XML_doc->createElement('errorStackTrace'));
    $ErrorStackTrace->appendChild($XML_doc->createTextNode($TestResultsArray->errorStackTrace));
  }
  $XMLFullDuration = $RootTestReport->appendChild($XML_doc->createElement('duration'));
  $XMLFullDuration->appendChild($XML_doc->createTextNode($FullDuration));
  $LongStdio = $RootTestReport->appendChild($XML_doc->createElement('keepLongStdio'));
  $LongStdio->appendChild($XML_doc->createTextNode("true"));
  $XML_doc->formatOutput = true;
  $XML_doc->save($output_file);
}


function RunTest($CurlVar, $URL, $Suite, $TestCase){
  $time_beg = microtime(true);
  $Request = GetRequest($URL, $Suite->request, $TestCase->params);
  echo $Request."<br>";
  try{
    $Response = SendRequest($CurlVar, $Request);
    $JSON_Response = ParseJSONResponse($Response);
    echo $JSON_Response."<br>";
    $Result = AssertTest($TestCase);
  }catch(Exception $e){
    $Result = Error($TestCase, $e->$Message);
  }
  $Result->suitename = $Suite->name;
  $Result->testname = $TestCase->name;
  $Result->duration = 0;
  $time_end = microtime(true);
  $Result->duration = $time_end - $time_beg;
  return $Result;
}
// ---------------------------- End of functions -------------------------------

// ---------------------------- Main Program -----------------------------------

$URL_str = "http://test.ils-glonass.ru";

$Full_time_beg = microtime(true);

$suites = ParseJSONTests(readtestfile("tests2.txt"));

$ch = create_Curl_Connect($URL_str);
$TestIndex = 0;
foreach ($suites as $suite){
  $tests = $suite->Tests;
  foreach ($tests as $test) {
    $test_results[$TestIndex] = RunTest($ch, $URL_str, $suite, $test);
    $TestIndex++;
  }
}

close_Curl_Connect($ch);

$Full_time_end = microtime(true);

$FullDuration = $Full_time_end - $Full_time_beg;

OutputToXMLLog("result", $test_results, $FullDuration);

?>
