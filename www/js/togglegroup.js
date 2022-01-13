// gray out filter values not in the filtered data
// message is 'id' and 'red', even though the color changes to gray (started with red)
Shiny.addCustomMessageHandler("togglegroup",
  function(message) { 
    // Select all input and span elements inside the grouping div's id
    var $cb = document.getElementById(message.id).getElementsByTagName('INPUT');
    var $sp = document.getElementById(message.id).getElementsByTagName('SPAN');
    for (var i = 0; i < $cb.length; i ++) {
      // evaluate whether the name should turn gray
      if (message.red.indexOf($cb[i].value) > -1) {
        $sp[i].className = 'graytext';
      }
      else
      {
        $sp[i].className = '';
      }
    }
  }
);