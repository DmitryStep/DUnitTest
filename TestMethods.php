<?php

// Return count of array's elements
function GetCount($ElementsArray, $args){
  $Items = $ElementsArray["Body"][0]["Items"];
  $DeepCounter = Count($args);
  for ($Index = 0; $Index < $DeepCounter; $Index++){
    foreach ($Items as $Item){
      if ($Item["id"] == $args[$Index]){
        $Items = $Item["Items"];
        break;
      }
    }
  }
  $Result = Count($Items);
  return $Result;
}

// Return count of fields in the element of array
function GetFieldsCount($ElementsArray, $args){
  $Result = 0;
  $Items = $ElementsArray["Body"][0]["Items"];
  $DeepCounter = Count($args);
  for ($Index = 0; $Index < $DeepCounter - 1; $Index++){
    foreach ($Items as $Item){
      if ($Item["id"] == $args[$Index]){
        $Items = $Item["Items"];
        break;
      }
    }
  }
  foreach ($Items as $Item){
    if ($Item["id"] == $args[$DeepCounter - 1]){
      $Result = Count($Item);
      break;
    }
  }
  return $Result;
}

// Return value of array's element
function GetValue($ElementsArray, $args){
  $Items = $ElementsArray["Body"][0]["Items"];
  $DeepCounter = Count($args);
  for ($Index = 0; $Index < $DeepCounter - 2; $Index++){
    foreach ($Items as $Item){
      if ($Item["id"] == $args[$Index]){
        $Items = $Item["Items"];
        break;
      }
    }
  }
  foreach ($Items as $Item){
    if ($Item["id"] == $args[$DeepCounter - 2]){
      $Result = $Item[$args[$DeepCounter - 1]];
      break;
    }
  }
  if ($Result == ""){
    $Result = "null";
  }
  return $Result;
}

?>