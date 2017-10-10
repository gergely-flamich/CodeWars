function solution(input, markers){
  var lines = input.split("\n");
  var stripped = [];
  
  for (var i = 0, line; line = lines[i]; i++){
    var strippedLine = []
    
    for (var j = 0, c = line[0]; (c = line[j]) && markers.indexOf(c) == -1 ; j++){
      strippedLine.push(c);
    }
    var strippedLn = strippedLine.join("").trim();
    stripped.push(strippedLn);
  }
  return stripped.join("\n");
}
