var url ='http://www.fussball.de/spiel/etus-haltern-bvh-dorsten/-/spiel/0243FOI8BS000000VS54898EVUVF00VA#!/';
var page = new WebPage()
var fs = require('fs');


page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write('1.html', page.content, 'w');
            phantom.exit();
    }, 2500);
}
