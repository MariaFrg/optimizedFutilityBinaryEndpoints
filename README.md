# optimizedFutility_binaryEndpoints
R Code for the corresponding manuscript: Freitag, M., Li, X. & Rauch, G. "Optimal Futility Stopping Boundaries for Binary Endpoints"

* **What the project does:**
This is the R code used for the calculation of optimized futility boundaries in the manuscript "Optimal Futility Stopping Boundaries for Binary Endpoints". It also calculates operating characteristics for the resulting designs, as well as for Simon's designs (Simon, R.: Optimal two-stage designs for phase ii clinical trials. (1989)) and Kim' modified versions of Simon's designs (Kim, J., Schell, M.: Modified simon’s minimax and optimal two-stage designs for single-arm phase ii cancer clinical trials. (2019)).

* **Why the project is useful:**
The introduced design allows for flexible non-binding futility boundaries for binary endpoints. It is ready to use with prespecified parameters to design single-arm clinical trials. It also offers analysis functions to compare the resulting operating characteristics of the design with with those of Simon's designs and Kim's modified Simon's designs.

* **How users can get started with the project:**
Clone the repository and run "calculate_twostage_OCs.R" and "search_opt_fut_boundary.R". You can then run the function calculate_optBoundaryOCs(p0, p1, pi_wrong, power_loss, alpha, beta, min1stage, max1stage) with the parameters you need and get the design and resulting operating characteristics. The other files create tables with performance characteristics for Simon's designs and Kim's modified Simon's designs. Some of them are used for the comparison in the manuscript.

* **Where users can get help with your project:**
See the above mentioned manuscript.

