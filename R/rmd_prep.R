options(width = 300)
# css argument does not work properly with toc argument. custom css-code is not placed after the default css-code.
cat("
<style type='text/css'>
    div.slide.titlepage {
      text-align: center;
      font-size: 150%;
    }

    body {
      padding: 20px;
      background-color: rgb(185,179,175)
    }
    
    h1, h2, h3, a {
      font-weight: normal;
      color: rgb(200, 0, 0);
      margin: 0px;
    }
    
    h1 {
      font-family: Georgia, Times, serif;
      font-size: 250%;
      text-shadow: 2px 2px 3px #666666;
      padding-bottom: 10px;
    }

    h2 {
      font-family: 'Gill Sans', Arial, sans-serif;
      font-size: 90%;
      text-transform: uppercase;
      letter-spacing: 0.2em;
    }

    h3 {
      font-size: 150%;
    }

    pre {
      background-color: rgb(215,219,215)
    }

    p {
      font-family: Arial, Verdana, sans-serif;
      line-height: 1.4em;
      color: #665544;
    }

    p.intro:first-line {
      font-weight: bold;
    }

    .credits {
      font-style: italic;	
      text-align: right;
    }

    a {
      text-decoration: none;
    }

    a:hover {
      text-decoration: underline;
    }

    body .main-container {
      max-width: 5000px; 
    }
    
</style>
    ")
