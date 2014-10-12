module Handler.Home where

import Import

{-

Yesod follows a naming convention for handler function names: the lower-cased
HTTP request method, followed by the route name. Therefore, the function for
HomeR's GET handler would be getHomeR.

Each handler function lives in the Handler monad, and has to return a value
that can be serialized over an HTTP connection. Two common examples of such
values are HTML and JSON data. In this case, we'll return Html.

-}
getHomeR :: Handler Html
-- defaultLayout uses the application's standard page layout to display
-- some contents. In our application, we're just using the standard
-- layout, which includes a basic HTML 5 page outline.
getHomeR = defaultLayout $ do
    -- Set the HTML <title> tag.
    setTitle "Yesod Web Service Homepage"

    -- Include some CDN-hosted Javascript and CSS to make our page a little nicer.
    addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"
    addStylesheetRemote "//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.min.css"

    -- Hamlet is the standard HTML templating language used by Yesod.
    -- In this case, we include some specific markup to take advantage of
    -- the bootstrap CSS we just included.
    -- For more information on Hamlet, please see:
    -- http://www.yesodweb.com/book/shakespearean-templates
    [whamlet|
        <div .container-fluid>
          <div .row-fluid>
            <h1>Welcome to the web service
        
          <div .row-fluid>
            <div .span6>
                <h2>Fibs
                <p>
                    Fib number
                    <input #fibinput type=number value=4>
                    is
                    <span #fiboutput>

            <div .span6>
                <h2>Binary
                <p>
                    Binary number
                    <input #bininput type=number value=4>
                    is
                    <span #binoutput>

            <div .span6>
                <h2>Test1
                <p>
                    Testing function of 1 variable
                    <input #test1input type=number value=7>
                    is
                    <span #test1output>

            <div .span6>
                <h2>Test2
                <p>
                    Testing function of 2 variables
                    <input #test2input1 type=number value=2>
                    <input #test2input2 type=number value=15>
                    is
                    <span #test2output>

            <div .span6>
                <h2>Test3
                <p>
                    Testing function of 3 variables
                    <input #test3input1 type=number value=2>
                    <input #test3input2 type=number value=7>
                    <input #test3input3 type=number value=15>
                    is
                    <span #test3output>

              |]
                    
{-            <div .span6>
            
                <h2>Markdown
                <textarea #markdowninput>
                    ## Welcome
                    
                    Welcome to the Markdown demo. __Markup__ should work *correctly*.
                <div .control-group>
                    <button #updatemarkdown .btn .btn-primary>Update markdown output
                <div #markdownoutput>
-}                    

    -- Similar to Hamlet, Yesod has Lucius for CSS, and Julius for Javascript.
    toWidget [lucius|
        body {
            margin: 0 auto;
        }
        
        #markdowninput {
            width: 100%;
            height: 300px;
        }
        
        #markdownoutput {
            border: 1px dashed #090;
            padding: 0.5em;
            background: #cfc;
        }
    |]
    toWidget [julius|
        function updateFib() {
            $.getJSON("/fib/" + $("#fibinput").val(), function (o) {
                $("#fiboutput").text(o.value);
            });
        }

        function updateBin() {
            $.getJSON("/binary/" + $("#bininput").val(), function (o) {
                $("#binoutput").text(o.value);
            });
        }
        
        function updateTest1() {
            $.getJSON("/test1/" + $("#test1input").val(), function (o) {
                $("#test1output").text(o.value);
            });
        }

        function updateTest2() {
            $.getJSON("/test2/" + $("#test2input1").val() + "/" + $("#test2input2").val(), function (o) {
                $("#test2output").text(o.value);
            });
        }

        function updateTest3() {
            $.getJSON("/test3/" + $("#test3input1").val() + "/" + $("#test3input2").val() + "/" + $("#test3input3").val(), function (o) {
                $("#test3output").text(o.value);
            });
        }
        
        function updateMarkdown() {
            // Note the use of the MarkdownR Haskell data type here.
            // This is an example of a type-safe URL.
            $.ajax("@{MarkdownR}", {
                data: {"markdown": $("#markdowninput").val()},
                success: function (o) {
                     $("#markdownoutput").html(o.html);
                },
                type: "PUT"
            });
        }
        
        $(function(){
            updateFib();
            $("#fibinput").change(updateFib);
            
            updateBin();
            $("#bininput").change(updateBin);
            
            updateTest1();
            $("#test1input").change(updateTest1);

            updateTest2();
            $("#test2input1").change(updateTest2);
            $("#test2input2").change(updateTest2);

            updateTest3();
            $("#test3input1").change(updateTest3);
            $("#test3input2").change(updateTest3);
            $("#test3input3").change(updateTest3);
            
            updateMarkdown();
            $("#updatemarkdown").click(updateMarkdown);
        });
    |]