//Returns a valid Binary Search Tree, built from the array passed as parameter.
function toBSTArray(array) {

	  if (arguments.length === 0) return undefined;

	  var bst = [];

    for(var i = 0, item; item = array[i]; i++){
        bst = insertNew(item, bst);
    }

	  return bst;
}

function insertNew(elem, array){
    if (array.length == 0) return [elem];

    var i = 0;

    while (array[i] !== undefined){
        i = (array[i] > elem) ? 2 * i + 1 : 2 * i + 2;
    }

    array[i] = elem;

    return array;
}

//Returns true is the array parameter is a valid BST, false otherwise.
function isBSTArray(array) {

	  if (arguments.length === 0) return false;

    var isBst = true;

    for (var i = 0; isBst && i < array.length; i++){
        var item = array[i];
        if (item === undefined) continue;

        var parent = ~~((i-1)/2);
        isBst = (array[2*i+1] === undefined || array[2*i+1] <= item) &&
            (array[2*i+2] === undefined || array[2*i+2] >= item) &&
            (i % 2 == 1 ? array[parent] >= item : array[parent] <= item);
    }

	  return isBst;
}

//Returns a linear sorted array without undefined values. The parameter must be a BSTArray
function toArray(BSTArray) {

	  if (!isBSTArray(BSTArray)) throw new Error('The parameter must be a BSTArray');

	  var result = [];

    var i = 0;

    while (true){
        if(BSTArray[2*i+1] !== undefined){
            i = 2 * i + 1;
            continue;
        }

        if(BSTArray[i] !== undefined) result.push(BSTArray[i]);
        BSTArray[i] = undefined;

        if(BSTArray[2*i + 2] !== undefined){
            i = 2 * i + 2;
            continue;
        }

        i = (i - 1) / 2;

        if (i < 0) break;

        i = ~~i;

    }

	  return result;
}
