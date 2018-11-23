options(width = 300)
# css argument does not work properly with toc argument. custom css-code is not placed after the default css-code.
cat("
<style type='text/css'>
body .main-container {
  max-width: 5000px; 
}
</style>
")
