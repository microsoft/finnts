## R CMD check results
There were no ERRORs or WARNINGs. 

There was 2 NOTES:

* checking dependencies in R code ... NOTE
  Imports includes 29 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

  This package does leverage many outside packages. The main feature of this package is 
  that it consolidates a lot of different models into one package to run them automatically. 
  So having many required packages is important to the package. 
  
* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Mike Tokic <mftokic@gmail.com>’

  New submission

  License components with restrictions and base license permitting such:
    MIT + file LICENSE
  File 'LICENSE':
      MIT License
  
      Copyright (c) Microsoft Corporation.
  
      Permission is hereby granted, free of charge, to any person obtaining a copy
      of this software and associated documentation files (the "Software"), to deal
      in the Software without restriction, including without limitation the rights
      to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
      copies of the Software, and to permit persons to whom the Software is
      furnished to do so, subject to the following conditions:
  
      The above copyright notice and this permission notice shall be included in all
      copies or substantial portions of the Software.
  
      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
      IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
      AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
      OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
      SOFTWARE
  
  The above changes to the MIT license are enforced by my company, Microsoft. 
  
  Also this is my first cran submission. 

## Downstream dependencies
There are currently no downstream dependencies for this package