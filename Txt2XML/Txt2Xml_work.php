<?php

// Convert string of file to array
function GetStringAsArray($String){
  $Result = explode("|", $String);
  $Num = 0;
  foreach ($Result as $Res){
    $Result[$Num] = ltrim(rtrim($Res));
    $Num++;
  }
  return $Result;
}


// Convert TestResult from txt to NUnit-format
function GetTestResult($TestResultStr){
  if ($TestResultStr == "OK!!!") {
    $Result = "Pass";
  }
  else
  if ($TestResultStr == "FAIL!") {
    $Result = "Failure";
  }
  else {
    $Result = "Error";
  }
  return $Result;
}

// Parsing of file with TestResults
function ParseTestResults($TestFile){
  $testdata = false;
  $StrNo = 0;

  $Fields = explode(",", "MODE,REPLAN,OPTIM,DIFF,COST,DIST,POINTS,ORDERS,TASKS");

  $File = fopen($TestFile, "r");
  while (!feof($File)) {
    $CurrentString = fgets($File);
    $CurrentString = rtrim($CurrentString);
    $CurrentString = rtrim($CurrentString, "|");
    $CurrentString = ltrim($CurrentString, "|");
    if (($CurrentString != "")&&($CurrentString != "*/")) {
      $TestResultArr = GetStringAsArray($CurrentString);
      if ($testdata) {
        $TestResults[$StrNo]->suitename = "ILS Logistics Tests";
        $TestName = $TestResultArr[0].".";
        for ($i = 0; $i < 9; $i++) {
          $TestName = $TestName." ".$Fields[$i]."=".$TestResultArr[$i + 2].".";
        }
        $TestResults[$StrNo]->testname = $TestName;
        $TestResults[$StrNo]->testresult = GetTestResult($TestResultArr[1]);
        $TestResults[$StrNo]->errorStackTrace = "";
        $TestResults[$StrNo]->duration = $TestResultArr[11];
        $StrNo++;
      }
      else {
        $testdata = (count($TestResultArr)==12)&&($TestResultArr[0] == "PATH");
      }
    }
  }
  fclose($File);
  return $TestResults;
}

// Output Test Results into log in JUnit-format
function OutputToXMLLog($output_file, $TestResultsArray, $StartDateTime){
  $failures = 0;
  $passed = 0;
  $errors = 0;
  $FullDuration = 0;

  $XML_doc = new DomDocument("1.0", "UTF-8");
  $TestReport = $XML_doc->appendChild($XML_doc->createElement('test-results'));
  $Attr = $XML_doc->CreateAttribute('time');
  $Attr->value = Date("h:m:s");
  $TestReport->appendChild($Attr);
  $Attr = $XML_doc->CreateAttribute('date');
  $Attr->value = Date("d.m.Y");
  $TestReport->appendChild($Attr);
  $Attr = $XML_doc->CreateAttribute('notrun');
  $Attr->value = 0;
  $TestReport->appendChild($Attr);
  $Attr = $XML_doc->CreateAttribute('total');
  $Attr->value = Count($TestResultsArray);
  $TestReport->appendChild($Attr);

    $TestsCount = 0;
    $SuiteDuration = 0;
    $Suite = $TestReport->appendChild($XML_doc->createElement('test-suite'));
    $Attr = $XML_doc->CreateAttribute('notrun');
    $Attr->value = 0;
    $Suite->appendChild($Attr);
    $Attr = $XML_doc->CreateAttribute('name');
    $Attr->value = "ILS Logistics Tests";
    $Suite->appendChild($Attr);
    $Results = $Suite->appendChild($XML_doc->createElement('results'));

    foreach ($TestResultsArray as $TestResult){
      $TestsCount++;
      $TestCase = $Results->appendChild($XML_doc->createElement('test-case'));
      $Attr = $XML_doc->CreateAttribute('time');
      $Attr->value = $TestResult->duration;
      $TestCase->appendChild($Attr);
    $FullDuration = $FullDuration + $TestResult->duration;
      $Attr = $XML_doc->CreateAttribute('name');
      $Attr->value = $TestResult->testname;
      $TestCase->appendChild($Attr);
      $Attr = $XML_doc->CreateAttribute('result');
      $Attr->value = $TestResult->testresult;
      $TestCase->appendChild($Attr);
      $Attr = $XML_doc->CreateAttribute('executed');
      $Attr->value = "true";
      $TestCase->appendChild($Attr);
      if ($TestResult->testresult == "Pass"){
        $passed++;
        $Attr = $XML_doc->CreateAttribute('success');
        $Attr->value = "true";
        $TestCase->appendChild($Attr);
      }else
      if ($TestResult->testresult == "Failure"){
        $failures++;
        $Attr = $XML_doc->CreateAttribute('success');
        $Attr->value = "false";
        $TestCase->appendChild($Attr);
        $Fail = $TestCase->appendChild($XML_doc->createElement('failure'));
        $Msg = $Fail->appendChild($XML_doc->createElement('message'));
        $Msg->appendChild($XML_doc->createTextNode($TestResult->errorStackTrace));
      }
      else{
        $errors++;
        $Attr = $XML_doc->CreateAttribute('success');
        $Attr->value = "false";
        $TestCase->appendChild($Attr);
        $Fail = $TestCase->appendChild($XML_doc->createElement('failure'));
        $Msg = $Fail->appendChild($XML_doc->createElement('message'));
        $Msg->appendChild($XML_doc->createTextNode($TestResult->errorStackTrace));
      }
    }
    $Attr = $XML_doc->CreateAttribute('total');
    $Attr->value = $TestsCount;
    $Suite->appendChild($Attr);

  $FullStat = $TestReport->appendChild($XML_doc->createElement('statistics'));
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('tests');
  $Attr->value = Count($TestResultsArray);
  $Stat->appendChild($Attr);
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('failures');
  $Attr->value = $failures;
  $Stat->appendChild($Attr);
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('errors');
  $Attr->value = $errors;
  $Stat->appendChild($Attr);
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('success-rate');
  if ((Count($TestResultsArray)) > 0) {
    $Attr->value = Round($passed * 100 / Count($TestResultsArray), 2);
  }
  else {
    $Attr->value = 100;
  }
  $Stat->appendChild($Attr);
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('started-at');
  $Attr->value = $StartDateTime;
  $Stat->appendChild($Attr);
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('finished-at');
  $Attr->value = $StartDateTime;
  $Stat->appendChild($Attr);
  $Stat = $FullStat->appendChild($XML_doc->createElement('Stat'));
  $Attr = $XML_doc->CreateAttribute('runtime');
  $Attr->value = $FullDuration;
  $Stat->appendChild($Attr);

  $XML_doc->formatOutput = true;
  $XML_doc->save($output_file);
}

$testfilename = "e:\\DEV_PHP\\Txt2XML\\tests.txt";
$XMLfilename = "e:\\DEV_PHP\\Txt2XML\\PHP-Result.xml";
if (file_exists($testfilename)){

  $StartDate = Date("d.m.Y h:m:s");

  $TestResults = ParseTestResults($testfilename);
  OutputToXMLLog($XMLfilename, $TestResults, $StartDate);
}

?>