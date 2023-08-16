# Functions for scripts to solve for equilibrium of the orbital location game, report statistics, make plots.

#####
# Physical and economic functions
#####

# minimum number of satellites needed at altitude h (numeric scalar). has the beam angle, phi, fixed to an average value.
# Units: satellites
Q_bar <- function(h) {
	phi = 0.4 # See calibration notes in SI
	(6371/(h * tan(phi/2)))^2
}

# service quality function. Takes a vector of named parameters as input and two variables: "speed" (bandwidth), S_i, and latency, L_i.
# Units: $/year
quality <- function(params, S_i, L_i) {
	# params$a_L * (params$L_bar - L_i) + params$a_S * sqrt(pmin(S_i, params$S_bar))
	params$a * (params$L_bar - L_i) * (S_i^2 / (S_i^2 + params$S_bar^2))
}

# Volume function for an orbital shell. Takes the height as an input and returns the volume of a spherical shell with thickness 35km. The "rho" here is shell thickness, what is now \Delta in the SI.
# Units: km^3
shell_volume <- function(h, rho=17.5) {
	4/3*pi*(6371+h+rho)^3 - 4/3*pi*(6371+h-rho)^3
}

# Velocity function for an orbital shell. Takes the height as an input and returns the velocity of an object at that altitude. From Wiki on orbital speed: "For orbits with small eccentricity, the length of the orbit is close to that of a circular one, and the mean orbital speed can be approximated either from observations of the orbital period and the semimajor axis of its orbit, or from knowledge of the masses of the two bodies and the semimajor axis." The formula given is v \approx sqrt(\mu/a), where \mu is the gravitational constant and a is the semimajor axis. The semimajor axis is the average distance from the center of the orbit to the center of the Earth, which is 6371 + h. The gravitational constant is 3.986004415e+5 km^3/s^2. (Source: https://en.wikipedia.org/wiki/Orbital_speed)
# Units: km/s
velocity <- function(h) {
	GM <- 3.986004415e+5 # gravitational constant in units of km^3/s^2
	sqrt(GM/(6371+h))
}

# Collision probability function for an object at altitude h. Takes params and height as an input and returns the probability of collision with another object in the same orbital shell.
# Units: conjunctions/instant
collision_prob <- function(params, h) {
	pi*velocity(h)*4*params$radius*params$radius/shell_volume(h, params$rho) # formula is pi*(v*(r_i + r_j)^2)/V, where v is the velocity, r_i and r_j are the radii of the two objects, and V is the volume of the orbital shell. 4*r^2 obtains when r_i = r_j = r.
}

# coverage function, \gamma in the notation. Takes only h and Q. beam angle, phi, and Earth's radius, R, are both fixed.
# Units: %
coverage <- function(h, Q) {
	phi = 0.4
	(Q >= Q_bar(h))*1 + (Q < Q_bar(h))*(h^2 * tan(phi/2)^2 * Q / 6371^2)
}

# size of the market served by one satellite. r(h,Q) in the Mathematica notebook. Bigger numbers worse for bandwidth since more customers allocated to fixed pipe size. Should be increasing in altitude (bigger beam circle) and decreasing in satellites (more overlap).
# Units: area (km^2)?
oneSatMarket <- function(h, Q) {
	phi = 0.4 # old value was 0.5
	pmin(h*tan(phi/2), 6371/sqrt(Q))
}

# Time of flight function for signal between consumer and satellite
# Units: ms
time_of_flight <- function(h, Q) {
	1e3*(h/(2*3e5))*
	(
			sqrt( 1 + (oneSatMarket(h,Q)/h)^2 ) +
			(h/oneSatMarket(h,Q))*
				log( oneSatMarket(h,Q)/h + 
					sqrt( 1 + (oneSatMarket(h,Q)/h)^2 )
				)
	)
}

# latency as a function of height, h, and number of satellites, Q. "R" here is the radius of the Earth, 6371 km, and "v" is the speed of light, 300000 km/s. v and R are hardcoded as 3e5 and 6371.
# Units: ms (s*1e3)
latency <- function(params, h, Q) {
	params$lambda*time_of_flight(h,Q) + params$mu
}

# "speed" (bandwidth) as a function of number of satellites, Q_i, and number of subscribers, D_i. There is a constant k that converts satellites/consumer into Mb/s, and a constant gamma_i which describes the number of consumers that can be served at the same time (percentage between 0-1). k is in the params vector.
# Units: Mb/s-person
bandwidth <- function(params, h_i, Q_i, h_j, Q_j, D_i, firm="L") {
	params$k*Q_i*(1 - congestion(params, h_i, h_j, Q_i, Q_j))/(coverage(h=h_i, Q=Q_i)*D_i)
}

# raw congestion as a function of quantity and proximity of objects, before wrapping in pmin. This is the "interference" function. The "own" firm is i, the "other" is j. The factor of 0.5 reflects that for any given close approach the two objects have an equal probability of maneuvering, i.e. symmetric congestion.
# Math notation: nL and nF in mathematica notebook Optimum_Linear.nb.
# Units: maneuvers/day
raw_congestion <- function(params, h_i, h_j, Q_i, Q_j) {

	Q_j <- Q_j*( abs(h_i - h_j) <= 35 ) # set Q_j = 0 if h_i and h_j are more than 35 km apart
	
	other_objects <- sum(params$objects_sh$objects[abs(params$objects_sh$h - h_i) <= 35], na.rm=TRUE)

	Q_j = Q_j + other_objects

	24*60*60*0.5*collision_prob(params, h_i)*Q_i*(
		Q_j + Q_i
	)
}

# congestion, wrapped in pmin. We make this distinction so that we have the raw_congestion number for use in calibration, and also don't have to change all the rest of the code written with "congestion" having pmin in it.
# Math notation: \beta_i in paper.
# Units: fraction of daily service time lost to avoidance maneuvers
congestion <- function(params, h_i, h_j, Q_i, Q_j) {
	Q_i <- pmax(Q_i, 1) # set Q_i = 1 if Q_i is less than 1
	pmin(
		params$tau*raw_congestion(params, h_i, h_j, Q_i, Q_j)/(24*Q_i),
		1)
}

# service availability as a function of time lost to avoidance maneuvers. Scaled by coverage area.
# Units: %
availability <- function(params, h_i, h_j, Q_i, Q_j) {
	pmin((1 - congestion(params, h_i, h_j, Q_i, Q_j))*Q_i/Q_bar(h_i),1) # symmetric interference, constellation average
}

# demand as a function of parameters. This is the solved function that assumes adaptive anticipations which are verified in equilibrium.
# Units: People (subscribers)
demand_AdAn <- function(params, firm="L") {
	(firm=="L")*(
		params$N*(2+params$theta_ubar)/3
		) +
	(firm=="F")*(
		params$N*(1-params$theta_ubar)/3
		)
}

# differences in max_wtp given service availability
# Units: $
wtp_diff <- function(params, h_L, h_F, Q_L, Q_F) {

	# availabilities
	alpha_L <- availability(params=params, h_i=h_L, h_j=h_F, Q_i=Q_L, Q_j=Q_F)
	alpha_F <- availability(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L)

	alpha_L <- u_availability(params, alpha_L)
	alpha_F <- u_availability(params, alpha_F)

	# bandwidths
	S_L <- bandwidth(params=params, h_i=h_L, h_j=h_F, Q_i=Q_L, Q_j=Q_F, D_i=demand_AdAn(params=params, "L"), firm="L")
	S_F <- bandwidth(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L, D_i=demand_AdAn(params=params, "F"), firm="F")

	# latencies
	L_L <- latency(params=params, h=h_L, Q=Q_L)
	L_F <- latency(params=params, h=h_F, Q=Q_F)

	# qualities
	x_L <- quality(params=params, S_i=S_L, L_i=L_L)
	x_F <- quality(params=params, S_i=S_F, L_i=L_F)

	alpha_L*x_L - alpha_F*x_F
}

# Function to calculate surplus of the lowest type purchasing from the follower
# Units: $
lowest_surplus <- function(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F) {
	# availabilities
	alpha_F <- availability(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L)

	alpha_F <- u_availability(params, alpha_F)

	# bandwidths
	S_F <- bandwidth(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L, D_i=demand_AdAn(params=params, "F"), firm="F")

	# latencies
	L_F <- latency(params=params, h=h_F, Q=Q_F)

	# qualities
	x_F <- quality(params=params, S_i=S_F, L_i=L_F)

	# price
	p_F <- price(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="F")

	alpha_F*params$theta_ubar*x_F - p_F
}

# service price as a function of parameters, height, and number of satellites. Takes an argument for whether computing for leader or follower. Assume Nash equilibrium here. theta_ubar is in params. "firm" is a character, either "L" for leader or "F" for follower.
# Units: $/person-year
price <- function(params, h_L, h_F, Q_L, Q_F, firm="L") {

	aLxL_aFxF = wtp_diff(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F)

	# prices
	(firm=="L")*(
		aLxL_aFxF*(2+params$theta_ubar)/3
		) +
	(firm=="F")*(
		aLxL_aFxF*(1-params$theta_ubar)/3
		)
}

# total cost function for a constellation as a function of altitude when both firms can choose height and number of satellites. Simplified from the earlier payoff function because now we can just plug in h and Q properly from the start.
# Units: $/year
sat_cost2 <- function(params, h, Q) {
		params$K*(params$c - params$d*h + 0.5*params$e*h^2) * Q
}

# payoff function for each player (profits) as function of parameters and grid variables. Takes leader/follower identity argument:  "firm" is a character, either "L" for leader or "F" for follower. This version is for when both firms can choose height and quantity.
# Units: $/year
payoff2  <- function(params, h_L, h_F, Q_L, Q_F, firm) {
	p = price(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm=firm)
	D = demand_AdAn(params=params, firm=firm)
	C = 	
	(firm=="L")*(
		sat_cost2(params=params, h=h_L, Q=Q_L)
		) +
	(firm=="F")*(
		sat_cost2(params=params, h=h_F, Q=Q_F)
		)
	p*D - C
}

# Function to calculate the minimum-cost full-coverage constellation
# Units: $/year
mincost_fullcoverage_const <- function(params) {
	h_grid <- data.frame(h=seq(100, 1500, by=1)) %>%
	mutate(
		Q = Q_bar(h),
		cost = sat_cost2(params=params, h=h, Q=Q)
	)

	minimizer <- h_grid[which.min(h_grid$cost), c("h", "Q", "cost")]

	return(minimizer)
}

# Function to raise calculated availabilities (alpha) to the power of a_U, which is in params.
# Units: unitless
u_availability <- function(params, alpha) {
	alpha^params$a_U
}

# Function to calculate the indifferent consumer in the Stackelberg equilibrium.
# Units: unitless
theta_star_calc <- function(params, h_L, h_F, Q_L, Q_F) {
	# availabilities
	alpha_L <- availability(params=params, h_i=h_L, h_j=h_F, Q_i=Q_L, Q_j=Q_F)
	alpha_F <- availability(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L)

	alpha_L <- u_availability(params=params, alpha=alpha_L)
	alpha_F <- u_availability(params=params, alpha=alpha_F)  

	# bandwidths
	S_L <- bandwidth(params=params, h_i=h_L, h_j=h_F, Q_i=Q_L, Q_j=Q_F, D_i=demand_AdAn(params=params, "L"), firm="L")
	S_F <- bandwidth(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L, D_i=demand_AdAn(params=params, "F"), firm="F")

	# latencies
	L_L <- latency(params=params, h=h_L, Q=Q_L)
	L_F <- latency(params=params, h=h_F, Q=Q_F)

	# qualities
	x_L <- quality(params=params, S_i=S_L, L_i=L_L)
	x_F <- quality(params=params, S_i=S_F, L_i=L_F)

	# prices
	p_L <- price(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="L")
	p_F <- price(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="F")

	# indifference
	theta_star <- (p_L - p_F)/(alpha_L*x_L - alpha_F*x_F)

	theta_star
} 

# consumer surplus function for L type under duopoly as a function of parameters and grid variables. This version is for when both firms can choose height and quantity.
# Units: $/year
consumer_surplus_2_L <- function(params, h_L, h_F, Q_L, Q_F, theta_star=NULL) {

	# availabilities
	alpha_L <- availability(params=params, h_i=h_L, h_j=h_F, Q_i=Q_L, Q_j=Q_F)

	alpha_L <- u_availability(params=params, alpha=alpha_L)

	# bandwidths
	S_L <- bandwidth(params=params, h_i=h_L, h_j=h_F, Q_i=Q_L, Q_j=Q_F, D_i=demand_AdAn(params=params, "L"), firm="L")

	# latencies
	L_L <- latency(params=params, h=h_L, Q=Q_L)

	# qualities
	x_L <- quality(params=params, S_i=S_L, L_i=L_L)

	# prices
	p_L <- price(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="L")

	CS_L <- alpha_L * x_L * 0.5 * ((1 + params$theta_ubar)^2 - theta_star^2) - p_L*(1 + params$theta_ubar - theta_star) - params$atmo_damages*Q_L
	
	CS_L

}

# consumer surplus function for F type under duopoly as a function of parameters and grid variables. This version is for when both firms can choose height and quantity.
# Units: $/year
consumer_surplus_2_F <- function(params, h_L, h_F, Q_L, Q_F, theta_star=NULL) {

	# availabilities
	alpha_F <- availability(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L)

	alpha_F <- u_availability(params=params, alpha=alpha_F)

	# bandwidths
	S_F <- bandwidth(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L, D_i=demand_AdAn(params=params, "F"), firm="F")

	# latencies
	L_F <- latency(params=params, h=h_F, Q=Q_F)

	# qualities
	x_F <- quality(params=params, S_i=S_F, L_i=L_F)

	# prices
	p_F <- price(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="F")

	CS_F <- alpha_F * x_F * 0.5 * (theta_star^2 - params$theta_ubar^2) - p_F*(theta_star - params$theta_ubar) - params$atmo_damages*Q_F

	CS_F

}

# consumer surplus function under duopoly as a function of parameters and grid variables. This version is for when both firms can choose height and quantity.
# Units: $/year
consumer_surplus_2 <- function(params, h_L, h_F, Q_L, Q_F) {

	consumer_surplus_2_L(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F) +	consumer_surplus_2_F(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F)

}

# social welfare under a single constellation.
# Units: $/year
swf_1const <- function(params, h, Q) {

	# bandwidths
	S <- bandwidth(params=params, h_i=h, h_j=0, Q_i=Q, Q_j=0, D_i=params$N)

	# latencies
	L <- latency(params=params, h=h, Q=Q)

	# qualities
	x <- quality(params=params, S_i=S, L_i=L)

	# availability
	alpha <- availability(params=params, h_i=h, h_j=0, Q_i=Q, Q_j=0)

	(params$theta_ubar + 0.5)*u_availability(params=params, alpha=alpha)*x*params$N - sat_cost2(params, h, Q) - params$atmo_damages*Q

}

# social welfare under a single constellation -- wrapper for GenSA. Since the GenSA algorithm minimizes, we multiply by -1.
# Units: Billion $/year
swf_1const_gensa <- function(h_Q_vec, params) {
	h <- h_Q_vec[1]
	Q <- h_Q_vec[2]
	-swf_1const(params=params, h=h, Q=Q)*1e-9
}

# social welfare under a single constellation -- wrapper for optim. optim maximization setting is at the function call.
# Units: Billion $/year
swf_1const_optim <- function(h_Q_vec, params) {
	h <- h_Q_vec[1]
	Q <- h_Q_vec[2]
	swf_1const(params=params, h=h, Q=Q)*1e-9
}

# social welfare under two constellations. This is consumer surplus in the duopoly case minus production costs -- assumes that firms price at cost.
# Units: $/year
swf_2const <- function(params, h_L, h_F, Q_L, Q_F, theta_star=theta_star_calc(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F)) {
	# availabilities
	alpha_L <- availability(params=params, h_i=h_L, h_j=h_F, Q_i=Q_L, Q_j=Q_F)
	alpha_F <- availability(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L)

	u_alpha_L <- u_availability(params=params, alpha=alpha_L)
	u_alpha_F <- u_availability(params=params, alpha=alpha_F)

	# bandwidths
	S_L <- bandwidth(params=params, h_i=h_L, h_j=h_F, Q_i=Q_L, Q_j=Q_F, D_i=(1.5 - theta_star)*params$N, firm="L")
	S_F <- bandwidth(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L, D_i=(theta_star - 0.5)*params$N, firm="F")

	# latencies
	L_L <- latency(params=params, h=h_L, Q=Q_L)
	L_F <- latency(params=params, h=h_F, Q=Q_F)

	# qualities
	x_L <- quality(params=params, S_i=S_L, L_i=L_L)
	x_F <- quality(params=params, S_i=S_F, L_i=L_F)

	TS_L <- (u_alpha_L * x_L * 0.5 * ((1 + params$theta_ubar)^2 - theta_star^2)) * params$N - sat_cost2(params, h_L, Q_L)
	TS_F <- (u_alpha_F * x_F * 0.5 * (theta_star^2 - params$theta_ubar^2)) * params$N - sat_cost2(params, h_F, Q_F)

	TS_L + TS_F - params$atmo_damages*(Q_L + Q_F)
}

# social welfare under two constellations -- gensa version. Since the GenSA algorithm minimizes, we multiply by -1.
# Units: Billion $/year
swf_2const_gensa <- function(h_Q_vec, params) {
	h_L <- h_Q_vec[1]
	Q_L <- h_Q_vec[2]
	h_F <- h_Q_vec[3]
	Q_F <- h_Q_vec[4]
	theta_star <- h_Q_vec[5]
	res <- swf_2const(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, theta_star=theta_star)*1e-9
	# The next two lines cover cases where the solver returns a value that is infinite or NA and direct the solver away from those inputs.
	if(is.infinite(res)) { res <- -1000}
	if(is.na(res)) { res <- -1000}
	-res
}

# social welfare under two constellations -- gensa version reformulated to set follower's height as a positive offset of the leader's. This resolves the numerical instability issue where the optimal constellation flips for small parameter changes---indicative of a multiplicity of solutions. For smoothness in sensitivity analyses, this reformulation sets the convention that optimal solutions where the second constellation is higher will be chosen. Since the GenSA algorithm minimizes, we multiply by -1.
# Units: Billion $/year
swf_2const_gensa_reform <- function(h_Q_vec, params) {
	h_L <- h_Q_vec[1]
	Q_L <- h_Q_vec[2]
	h_F <- h_Q_vec[1] + h_Q_vec[3]
	Q_F <- h_Q_vec[4]
	theta_star <- h_Q_vec[5]
	res <- swf_2const(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, theta_star=theta_star)*1e-9
	# The next two lines cover cases where the solver returns a value that is infinite or NA and direct the solver away from those inputs.
	if(is.infinite(res)) { res <- -1000}
	if(is.na(res)) { res <- -1000}
	-res
}

#####
# Solvers and grid makers
#####

# Calibration function to solve for the annualized unit cost, which is what we call c(h). The unit cost of a satellite launched to 530 km should be $500,000. So then the annualized cost should be $100,000. This function makes the annualized unit cost equal to the target cost ($100,000).
# Units: $/sat/year
c_solver <- function(h=530, d, e, K=1, target_cost=100000) {
	res <- optim(c(1e5), fn=function(c,d,h,e,target_cost) (K*(c - d*h + 0.5*e*h^2) - target_cost)^2, method="Brent", lower=0, upper=5e6, d=d, h=h, e=e, target_cost=1e5, control=list(maxit=1000))
	
	message("Calibrated c is ",res$par, ". C(",h,") under this c is ", res$par - d*h + 0.5*e*h^2)

	res$par
}

# Function to make a payoff grid when both firms can choose height and quantity.
payoff_grid_maker2 <- function(params, hQ2_grid) {
	hQ2_grid %>%
		mutate(
			profit_L = payoff2(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="L")*1e-9,
			profit_F = payoff2(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="F")*1e-9,
			wtp_diff = wtp_diff(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F),
			lowest_surplus = lowest_surplus(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F),
			swf_1 = swf_1const(params=params, h_L=h_L, h_F=0, Q_L=Q_L, Q_F=0)*1e-9,
			swf_2 = swf_2const(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F)*1e-9
			) %>%
		filter(wtp_diff>0, lowest_surplus >=0 )
}

# Function to make a payoff grid when both firms can choose height and quantity -- data.table version
payoff_grid_maker2_dt <- function(params, hQ2_grid) {
	hQ2_grid <- data.table(hQ2_grid)
	hQ2_grid[, profit_L := payoff2(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="L")*1e-9]
	hQ2_grid[, profit_F := payoff2(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="F")*1e-9]
	hQ2_grid[, wtp_diff := wtp_diff(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F)]
	hQ2_grid[, lowest_surplus := lowest_surplus(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F)]
	hQ2_grid[, swf_2 := swf_2const(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F)*1e-9]
	hQ2_grid[wtp_diff > 0 & lowest_surplus >= 0]
}

# Function to augment the payoff grid with information about costs and prices along best-response choices
augment_payoff_grid <- function(params, payoffs) {
	payoffs_aug <- payoffs %>%
		mutate(
			# Leader costs and prices
			cost_L = sat_cost2(params, Q_L, h_L),
			price_L = price(params, Q_L, h_L, Q_F, h_F, firm="L"),
			# Follower costs and prices
			cost_F = sat_cost2(params, Q_F, h_F),
			price_F = price(params, Q_F, h_F, Q_L, h_L, firm="F"),
			# Leader and follower availability
			availability_L = availability(params, h_i=Q_L, h_j=Q_F, Q_i=h_L, Q_j=h_F),
			availability_F = availability(params, h_i=Q_F, h_j=Q_L, Q_i=h_F, Q_j=h_L)
		)
	return(payoffs_aug)
}

# Function to solve for a Stackelberg equilibrium where both firms can choose height and quantity of satellites. Uses payoff_grid computed from hQ2_grid, which is a grid of (h_F, Q_F, h_L, Q_L) combinations.
solve_model_dt_2 <- function(payoff_grid, data_report=TRUE) {

	## Construct the payoff grids, identify best-response designs, and select Stackelberg equilibrium. dplyr version
	payoff_grid <- payoff_grid %>%
		group_by(L_designs) %>%
		mutate(
			best_F_design = which.max(profit_F),
			best_F_design_ind = F_designs[best_F_design]==F_designs ) %>%
		ungroup() %>% 
		group_by(F_designs) %>%
		mutate(
			best_L_design = which.max(profit_L),
			best_L_design_ind = L_designs[best_L_design]==L_designs) %>% 
		ungroup() %>%
		mutate(
			equilibrium_candidates = best_L_design_ind*best_F_design_ind,
			eqm_profit_L = equilibrium_candidates*profit_L,
			equilibrium = eqm_profit_L == max(eqm_profit_L)
			) %>%
		data.table()


	F_design <- payoff_grid[equilibrium == 1, c(h_F, Q_F)]
	L_design <- payoff_grid[equilibrium == 1, c(h_L, Q_L)]
	names(F_design) <- c("h", "Q")
	names(L_design) <- c("h", "Q")

	if(data_report==TRUE) {out <- list(L=L_design, F=F_design, full_data=payoff_grid)}
	if(data_report==FALSE) {out <- list(L=L_design, F=F_design)}

	return(out)

}

# Function to solve for an optimal one constellation profile -- optim version
optim_1const <- function(params, h, Q) {
	result <- optim(c(250, 1000), fn=swf_1const_optim, params=params, method="L-BFGS-B", lower=c(200, 400), upper=c(1250, 500000), control=list(trace=1, maxit=1000, fnscale=-1))

	return(list(h=result$par[1], Q=result$par[2], swf=result$value))
}

# Function to solve for an optimal two constellation profile
optim_2const_dt <- function(payoff_grid, profit_constraint=FALSE) {

	# Constrain profits for both firms to be non-negative
	if(profit_constraint==TRUE) {
		payoff_grid <- payoff_grid[payoff_grid[, profit_L] >= 0 & payoff_grid[, profit_F] >= 0]
	}

	best_2const <- which.max(payoff_grid[, swf_2])

	opt2_L <- c(h=payoff_grid[best_2const, h_L], Q=payoff_grid[best_2const, Q_L])
	opt2_F <- c(h=payoff_grid[best_2const, h_F], Q=payoff_grid[best_2const, Q_F])

	return(list(L=opt2_L, F=opt2_F))
}

#####
# Reporting and plotting functions
#####

# Function to compute and return payoffs/surpluses when both firms can choose height and quantity
report_surplus_2 <- function(params, eqm, opt) {

	h_L <- eqm$L[1]
	Q_L <- eqm$L[2]
	h_F <- eqm$F[1]
	Q_F <- eqm$F[2]

	h_L_opt <- opt$L[1]
	Q_L_opt <- opt$L[2]
	h_F_opt <- opt$F[1]
	Q_F_opt <- opt$F[2]
	
	L_surplus <- payoff2(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="L")*1e-9
	F_surplus <- payoff2(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="F")*1e-9
	C_surplus <- consumer_surplus_2(params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F)*params$N*1e-9
	eqm_total_surplus <- L_surplus + F_surplus + C_surplus

	opt2_surplus <- swf_2const(params, h_L=h_L_opt, h_F=h_F_opt, Q_L=Q_L_opt, Q_F=Q_F_opt)*1e-9

	return(t(data.frame(
		L_surplus=L_surplus,
		F_surplus=F_surplus,
		C_surplus=C_surplus,
		total_surplus=eqm_total_surplus,
		opt2_surplus=opt2_surplus,
		welfare_loss_dol=opt2_surplus-eqm_total_surplus,
		welfare_loss_pct=(1-eqm_total_surplus/opt2_surplus)*100
		)))
}

# Function to compute and return product qualities when both leader and follower can choose height and size
report_qualities_2 <- function(params, eqm, opt1, opt2) {
	
	h_L <- eqm$L[1]
	Q_L <- eqm$L[2]
	h_F <- eqm$F[1]
	Q_F <- eqm$F[2]

	h_opt1 <- opt1[1]
	Q_opt1 <- opt1[2]

	h_L_opt2 <- opt2[1]
	Q_L_opt2 <- opt2[2]
	h_F_opt2 <- opt2[3]
	Q_F_opt2 <- opt2[4]
	theta_star_opt2 <- opt2[5]

	min_const <- mincost_fullcoverage_const(params)
	h_min <- min_const$h
	Q_min <- min_const$Q
	
	# bandwidths
	# eqm
	S_L <- bandwidth(params=params, h_i=h_L, h_j=h_F, Q_i=Q_L, Q_j=Q_F, D_i=demand_AdAn(params=params, "L"))
	S_F <- bandwidth(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L, D_i=demand_AdAn(params=params, "F"))
	# opt 1 constellation
	S_opt1 <- bandwidth(params=params, h_i=h_opt1, h_j=0, Q_i=Q_opt1, Q_j=0, D_i=params$N)
	# opt 2 constellations
	S_L_opt2 <- bandwidth(params=params, h_i=h_L_opt2, h_j=h_F_opt2, Q_i=Q_L_opt2, Q_j=Q_F_opt2, D_i=params$N*(1.5 - theta_star_opt2)) # Divide by 1.5 since demand is over [0.5, 1.5].
	S_F_opt2 <- bandwidth(params=params, h_i=h_F_opt2, h_j=h_L_opt2, Q_i=Q_F_opt2, Q_j=Q_L_opt2, D_i=params$N*(theta_star_opt2 - 0.5))
	# min cost constellation
	S_min <- bandwidth(params=params, h_i=h_min, h_j=0, Q_i=Q_min, Q_j=0, D_i=params$N)

	# latencies
	# eqm
	L_L <- latency(params=params, h=h_L, Q=Q_L)
	L_F <- latency(params=params, h=h_F, Q=Q_F)
	# opt 1 constellation
	L_opt1 <- latency(params=params, h=h_opt1, Q=Q_opt1)
	# opt 2 constellations
	L_L_opt2 <- latency(params=params, h=h_L_opt2, Q=Q_L_opt2)
	L_F_opt2 <- latency(params=params, h=h_F_opt2, Q=Q_F_opt2)
	# min cost constellation
	L_min <- latency(params=params, h=h_min, Q=Q_min)

	# congestions
	# eqm
	C_L <- congestion(params=params, h_i=h_L, h_j=h_F, Q_i=Q_L, Q_j=Q_F)
	C_F <- congestion(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L)
	# opt 1 constellation
	C_opt1 <- congestion(params=params, h_i=h_opt1, h_j=0, Q_i=Q_opt1, Q_j=0)
	# opt 2 constellations
	C_L_opt2 <- congestion(params=params, h_i=h_L_opt2, h_j=h_F_opt2, Q_i=Q_L_opt2, Q_j=Q_F_opt2)
	C_F_opt2 <- congestion(params=params, h_i=h_F_opt2, h_j=h_L_opt2, Q_i=Q_F_opt2, Q_j=Q_L_opt2)
	# min cost constellation
	C_min <- congestion(params=params, h_i=h_min, h_j=0, Q_i=Q_min, Q_j=0)

	# maneuver burden per satellite in each system -- units of maneuvers/sat-day within each system
	# eqm
	maneuvers_L <- raw_congestion(params=params, h_i=h_L, h_j=h_F, Q_i=Q_L, Q_j=Q_F)
	maneuvers_F <- raw_congestion(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L)
	# opt 1 constellation
	maneuvers_opt1 <- raw_congestion(params=params, h_i=h_opt1, h_j=0, Q_i=Q_opt1, Q_j=0)
	# opt 2 constellations
	maneuvers_L_opt2 <- raw_congestion(params=params, h_i=h_L_opt2, h_j=h_F_opt2, Q_i=Q_L_opt2, Q_j=Q_F_opt2)
	maneuvers_F_opt2 <- raw_congestion(params=params, h_i=h_F_opt2, h_j=h_L_opt2, Q_i=Q_F_opt2, Q_j=Q_L_opt2)
	# min cost constellation
	maneuvers_min <- raw_congestion(params=params, h_i=h_min, h_j=0, Q_i=Q_min, Q_j=0)

	# average maneuver burden per satellite across constellations in each system -- units of maneuvers/satellite-day across constellations in each system
	# duopoly
	avg_maneuvers_duopoly <- (maneuvers_L + maneuvers_F)/(Q_L + Q_F)
	# opt 1 constellation
	avg_maneuvers_opt1 <- maneuvers_opt1/Q_opt1
	# opt 2 constellation
	avg_maneuvers_opt2 <- (maneuvers_L_opt2 + maneuvers_F_opt2)/(Q_L_opt2 + Q_F_opt2)
	# min cost constellation
	avg_maneuvers_min <- maneuvers_min/Q_min

	# availabilities
	# eqm
	alpha_L <- availability(params=params, h_i=h_L, h_j=h_F, Q_i=Q_L, Q_j=Q_F)
	alpha_F <- availability(params=params, h_i=h_F, h_j=h_L, Q_i=Q_F, Q_j=Q_L)
	# opt 1 constellation
	alpha_opt1 <- availability(params=params, h_i=h_opt1, h_j=0, Q_i=Q_opt1, Q_j=0)
	# opt 2 constellations
	alpha_L_opt2 <- availability(params=params, h_i=h_L_opt2, h_j=h_F_opt2, Q_i=Q_L_opt2, Q_j=Q_F_opt2)
	alpha_F_opt2 <- availability(params=params, h_i=h_F_opt2, h_j=h_L_opt2, Q_i=Q_F_opt2, Q_j=Q_L_opt2)
	# min cost constellation
	alpha_min <- availability(params=params, h_i=h_min, h_j=0, Q_i=Q_min, Q_j=0)

	# qualities
	# eqm
	x_L <- quality(params=params, S_i=S_L, L_i=L_L)
	x_F <- quality(params=params, S_i=S_F, L_i=L_F)
	# opt 1 constellation
	x_opt1 <- quality(params=params, S_i=S_opt1, L_i=L_opt1)
	# opt 2 constellations
	x_L_opt2 <- quality(params=params, S_i=S_L_opt2, L_i=L_L_opt2)
	x_F_opt2 <- quality(params=params, S_i=S_F_opt2, L_i=L_F_opt2)
	# min cost constellation
	x_min <- quality(params=params, S_i=S_min, L_i=L_min)

	# prices
	# eqm
	p_L <- price(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="L")
	p_F <- price(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="F")
	# opt 1 constellation
	p_opt1 <- price(params=params, h_L=h_opt1, h_F=0, Q_L=Q_opt1, Q_F=0, firm="L")
	# opt 2 constellations
	p_L_opt2 <- price(params=params, h_L=h_L_opt2, h_F=h_F_opt2, Q_L=Q_L_opt2, Q_F=Q_F_opt2, firm="L")
	p_F_opt2 <- price(params=params, h_L=h_L_opt2, h_F=h_F_opt2, Q_L=Q_L_opt2, Q_F=Q_F_opt2, firm="F")
	# min cost constellation
	p_min <- price(params=params, h_L=h_min, h_F=0, Q_L=Q_min, Q_F=0, firm="L")

	# costs
	# eqm
	cost_L <- sat_cost2(params=params, h=h_L, Q=Q_L)
	cost_F <- sat_cost2(params=params, h=h_F, Q=Q_F)
	# opt 1 constellation
	cost_opt1 <- sat_cost2(params=params, h=h_opt1, Q=Q_opt1)
	# opt 2 constellations
	cost_L_opt2 <- sat_cost2(params=params, h=h_L_opt2, Q=Q_L_opt2)
	cost_F_opt2 <- sat_cost2(params=params, h=h_F_opt2, Q=Q_F_opt2)
	# min cost constellation
	cost_min <- sat_cost2(params=params, h=h_min, Q=Q_min)

	# coverages
	# eqm
	coverage_L <- coverage(h=h_L, Q=Q_L)
	coverage_F <- coverage(h=h_F, Q=Q_F)
	# opt 1 constellation
	coverage_opt1 <- coverage(h=h_opt1, Q=Q_opt1)
	# opt 2 constellations
	coverage_L_opt2 <- coverage(h=h_L_opt2, Q=Q_L_opt2)
	coverage_F_opt2 <- coverage(h=h_F_opt2, Q=Q_F_opt2)
	# min cost constellation
	coverage_min <- coverage(h=h_min, Q=Q_min)

	# indifference
	theta_star <- theta_star_calc(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F)

	# cutoff type preference
	cutoff_pref_L <- theta_star*u_availability(params, alpha_L) * x_L - p_L
	cutoff_pref_F <- theta_star*u_availability(params, alpha_F) * x_F - p_F
	cutoff_pref_L_opt2 <- theta_star_opt2*u_availability(params, alpha_L_opt2) * x_L_opt2 - p_L_opt2
	cutoff_pref_F_opt2 <- theta_star_opt2*u_availability(params, alpha_F_opt2) * x_F_opt2 - p_F_opt2

	# system profits
	L_surplus <- payoff2(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="L")*1e-9
	F_surplus <- payoff2(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, firm="F")*1e-9
	L_surplus_opt2 <- payoff2(params=params, h_L=h_L_opt2, h_F=h_F_opt2, Q_L=Q_L_opt2, Q_F=Q_F_opt2, firm="L")*1e-9
	F_surplus_opt2 <- payoff2(params=params, h_L=h_L_opt2, h_F=h_F_opt2, Q_L=Q_L_opt2, Q_F=Q_F_opt2, firm="F")*1e-9

	# markups from orbital misallocation
	L_markup <- ((p_L - p_L_opt2)/p_L_opt2)*100
	F_markup <- ((p_F - p_F_opt2)/p_F_opt2)*100

	# consumer surpluses
	L_surplus_consumer <- consumer_surplus_2_L(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, theta_star=theta_star)*params$N*1e-9
	F_surplus_consumer <- consumer_surplus_2_F(params=params, h_L=h_L, h_F=h_F, Q_L=Q_L, Q_F=Q_F, theta_star=theta_star)*params$N*1e-9
	L_surplus_consumer_opt2 <- consumer_surplus_2_L(params=params, h_L=h_L_opt2, h_F=h_F_opt2, Q_L=Q_L_opt2, Q_F=Q_F_opt2, theta_star=theta_star_opt2)*params$N*1e-9
	F_surplus_consumer_opt2 <- consumer_surplus_2_F(params=params, h_L=h_L_opt2, h_F=h_F_opt2, Q_L=Q_L_opt2, Q_F=Q_F_opt2, theta_star=theta_star_opt2)*params$N*1e-9

	# system-level total surplus
	total_surplus_L <- L_surplus + L_surplus_consumer
	total_surplus_F <- F_surplus + F_surplus_consumer
	total_surplus_opt1 <- swf_1const(params=params, h=h_opt1, Q=Q_opt1)*1e-9
	total_surplus_L_opt2 <- L_surplus_opt2 + L_surplus_consumer_opt2
	total_surplus_F_opt2 <- F_surplus_opt2 + F_surplus_consumer_opt2
	total_surplus_opt2 <- abs(opt2[6])
	total_surplus_min <-  swf_1const(params=params, h=h_min, Q=Q_min)*1e-9

	# system-level welfare loss
	welfare_loss_pct_L=(1-total_surplus_L/total_surplus_L_opt2)*100
	welfare_loss_pct_F=(1-total_surplus_F/total_surplus_F_opt2)*100
	welfare_loss_dol_L=total_surplus_L_opt2-total_surplus_L
	welfare_loss_dol_F=total_surplus_F_opt2-total_surplus_F

	# global welfare loss compared to best option
	welfare_loss_eqm <- (1 - (total_surplus_L + total_surplus_F)/max(total_surplus_opt1, total_surplus_L_opt2 + total_surplus_F_opt2))*100
	welfare_loss_opt2 <- (1 - (total_surplus_L_opt2 + total_surplus_F_opt2)/max(total_surplus_opt1, total_surplus_L_opt2 + total_surplus_F_opt2))*100
	welfare_loss_opt1 <- (1 - (total_surplus_opt1)/max(total_surplus_opt1, total_surplus_L_opt2 + total_surplus_F_opt2))*100
	welfare_loss_min <- (1 - (total_surplus_min)/max(total_surplus_opt1, total_surplus_L_opt2 + total_surplus_F_opt2))*100

	figures_of_merit <-	t(data.frame(
		altitude=c(h_L, h_F, h_L_opt2, h_F_opt2, h_opt1, h_min),
		size=c(Q_L, Q_F, Q_L_opt2, Q_F_opt2, Q_opt1, Q_min),
		latency=c(L_L, L_F, L_L_opt2, L_F_opt2, L_opt1, L_min),
		bandwidth=c(S_L, S_F, S_L_opt2, S_F_opt2, S_opt1, S_min),
		coverage=c(coverage_L, coverage_F, coverage_L_opt2, coverage_F_opt2, coverage_opt1, coverage_min),
		congestion=c(C_L, C_F, C_L_opt2, C_F_opt2, C_opt1, C_min),
		maneuvers=c(maneuvers_L, maneuvers_F, maneuvers_L_opt2, maneuvers_F_opt2, maneuvers_opt1, maneuvers_min),
		avg_maneuvers=c(avg_maneuvers_duopoly, NA, avg_maneuvers_opt2, NA, avg_maneuvers_opt1, avg_maneuvers_min),
		availability=c(alpha_L, alpha_F, alpha_L_opt2, alpha_F_opt2, alpha_opt1, alpha_min),
		quality_index=c(x_L, x_F, x_L_opt2, x_F_opt2, x_opt1, x_min),
		available_quality=c(x_L*alpha_L, x_F*alpha_F, x_L_opt2*alpha_L_opt2, x_F_opt2*alpha_F_opt2, x_opt1*alpha_opt1, x_min*alpha_min),
		min_theta=c(params$theta_ubar, params$theta_ubar, params$theta_ubar, params$theta_ubar, params$theta_ubar, params$theta_ubar),
		indifferent_theta=c(theta_star, theta_star, theta_star_opt2, theta_star_opt2, 1+params$theta_ubar, NA),
		max_theta=c(1+params$theta_ubar, 1+params$theta_ubar, 1+params$theta_ubar, 1+params$theta_ubar, 1+params$theta_ubar, 1+params$theta_ubar),
		prices=c(p_L, p_F, p_L_opt2, p_F_opt2, NA, NA),
		costs=c(cost_L, cost_F, cost_L_opt2, cost_F_opt2, cost_opt1, cost_min),
		profits=c(L_surplus, F_surplus, L_surplus_opt2, F_surplus_opt2, NA, NA),
		price_distortion=c(L_markup, F_markup, 0, 0, NA, NA),
		c_surplus=c(L_surplus_consumer, F_surplus_consumer, L_surplus_consumer_opt2, F_surplus_consumer_opt2, NA, NA),
		system_surplus=c(total_surplus_L, total_surplus_F, total_surplus_L_opt2, total_surplus_F_opt2, NA, NA),
		total_surplus=c(total_surplus_L + total_surplus_F, NA, total_surplus_opt2, NA, total_surplus_opt1, total_surplus_min),
		welfare_loss_pct=c(welfare_loss_pct_L, welfare_loss_pct_F, 0, 0, NA, NA),
		welfare_loss_dol=c(welfare_loss_dol_L, welfare_loss_dol_F, 0, 0, NA, NA),
		total_welfare_loss_pct=c(welfare_loss_eqm, welfare_loss_eqm, welfare_loss_opt2, welfare_loss_opt2, welfare_loss_opt1, welfare_loss_min),
		c_surplus_gain=c(0, 0, (L_surplus_consumer_opt2-L_surplus_consumer), (F_surplus_consumer_opt2-F_surplus_consumer), NA, NA),
		cutoff_pref=c(cutoff_pref_L, cutoff_pref_F, cutoff_pref_L_opt2, cutoff_pref_F_opt2, NA, NA)
		))
	colnames(figures_of_merit) = c("L", "F", "opt2_L", "opt2_F", "opt1", "min")

	return(figures_of_merit)
}

# Function to output certain key economic indicators in dollar and percentage units
report_indicators <- function(params, qualities) {

	# Convert qualities to data.frame if it isn't already
	if(!is.data.frame(qualities)) qualities <- as.data.frame(qualities)

	outcome_labels <- rownames(qualities)
	rownames(qualities) <- NULL
	qualities <- cbind(outcome=outcome_labels, qualities)


	welfare_perc_loss <- qualities %>%
		filter(outcome=="system_surplus") %>% 
		mutate(total_loss=(1 - (L+F)/(opt2_L+opt2_F))*100 ) %>%
		select(total_loss) %>% unlist() %>% as.numeric()
	
	welfare_dollar_loss <- qualities %>%
		filter(outcome=="system_surplus") %>% 
		mutate(total_loss=((opt2_L+opt2_F)-(L+F))*1e+9) %>%
		select(total_loss) %>% unlist() %>% as.numeric()

	avg_price_distortion <- qualities %>%
		filter(outcome=="price_distortion") %>% 
		select(L, F) %>%
		mutate(avg_price_distortion=(L+F)/2) %>%
		select(avg_price_distortion) %>% unlist() %>% as.numeric()

	consumer_surplus_gain <- qualities %>%
		filter(outcome=="c_surplus_gain") %>% 
		select(opt2_L, opt2_F) %>%
		mutate(total_consumer_surplus_gain=(opt2_L+opt2_F)*1e+9) %>%
		select(total_consumer_surplus_gain) %>% unlist() %>% as.numeric()

	L_consumer_surplus_gain <- qualities %>%
		filter(outcome=="c_surplus_gain") %>% 
		select(opt2_L) %>%
		mutate(total_consumer_surplus_gain=opt2_L*1e+9) %>%
		select(total_consumer_surplus_gain) %>% unlist() %>% as.numeric()

	F_consumer_surplus_gain <- qualities %>%
		filter(outcome=="c_surplus_gain") %>% 
		select(opt2_F) %>%
		mutate(total_consumer_surplus_gain=opt2_F*1e+9) %>%
		select(total_consumer_surplus_gain) %>% unlist() %>% as.numeric()

	profit_change_perc <- qualities %>%
		filter(outcome=="profits") %>%
		mutate(
			competitive_profits = L+F,
			optimal_profits = opt2_L+opt2_F,
			profit_change = (optimal_profits - competitive_profits)/competitive_profits*100
			) %>% select(profit_change) %>% unlist() %>% as.numeric()

	profit_change_dollar <- qualities %>%
		filter(outcome=="profits") %>%
		mutate(
			competitive_profits = L+F,
			optimal_profits = opt2_L+opt2_F,
			profit_change = (optimal_profits - competitive_profits)*1e+9
			) %>% select(profit_change) %>% unlist() %>% as.numeric()

	message(
		"Total duopoly welfare loss: ", round(welfare_perc_loss,2), "% or $", round(welfare_dollar_loss*1e-9,2), " billion.\n
		Average price distortion: ", round(avg_price_distortion,0), "%.\n
		Total consumer surplus gain from coordination: $", round(consumer_surplus_gain*1e-9,2), " billion or $", round(consumer_surplus_gain/params$N,0), " per person/year.\n
		Gain for Leader's subscribers: $", round(L_consumer_surplus_gain*1e-9,2), " billion.\n
		Gain for Follower's subscribers: $", round(F_consumer_surplus_gain*1e-9,2), " billion.\n
		Average change in profits from coordination: ", round(profit_change_perc,2), "% or $", round(profit_change_dollar*1e-9,2), " billion.\n"
	)
}

# Function to plot model payoffs for follower and leader in the Stackelberg game.
plot_payoffs <- function(params, hQ_grid) {
	payoff_grid <- payoff_grid_maker(params, hQ_grid)

	eqm <- solve_model_dt(params, hQ_grid)
	opt1 <- optim_1const_dt(params, hQ_grid)

	follower_payoff_grid_plot <- ggplot(payoff_grid, aes(x=Q, y=h)) +
		theme(text = element_text(size=20, family="Arial")) +
		geom_tile(aes(fill=profit_F), alpha = 0.25) +
		geom_contour(aes(z=profit_F, color=profit_F)) + 
		geom_point(aes(x=eqm$Q_star, y=eqm$h_star), size=3, color="firebrick4") +
		geom_point(aes(x=opt1$Q_opt, y=opt1$h_opt), size=3, color="black") +
		labs(title = "Follower payoff", y = "Follower altitude [km]", x="Leader size [sats]", fill="Profits")


	leader_payoff_grid_plot <- ggplot(payoff_grid, aes(x=Q, y=h)) +
		theme(text = element_text(size=20, family="Arial")) +
		geom_tile(aes(fill=profit_L), alpha = 0.25) +
		geom_contour(aes(z=profit_L, color=profit_L)) + 
		geom_point(aes(x=eqm$Q_star, y=eqm$h_star), size=3, color="firebrick4") +
		geom_point(aes(x=opt1$Q_opt, y=opt1$h_opt), size=3, color="black") +
		labs(title = "Leader payoff", y = "Follower altitude [km]", x="Leader size [sats]", fill="Profits")

	swf1_payoff_plot <- ggplot(payoff_grid, aes(x=Q, y=h)) +
		theme(text = element_text(size=20, family="Arial")) +
		geom_tile(aes(fill=swf_1), alpha = 0.25) +
		geom_contour(aes(z=swf_1, color=swf_1)) + 
		geom_point(aes(x=eqm$Q_star, y=eqm$h_star), size=3, color="firebrick4") +
		geom_point(aes(x=opt1$Q_opt, y=opt1$h_opt), size=3, color="black") +
		labs(title = "Total surplus -- 1 constellation", y = "Constellation altitude [km]", x="Constellation size [sats]", fill="Total surplus")

	return(list(follower=follower_payoff_grid_plot, leader=leader_payoff_grid_plot, swf1=swf1_payoff_plot))
}


# Function to plot model payoffs for follower and leader when both leader and follower can choose height and size
plot_payoffs_2 <- function(params, hQ2_grid, payoff_grid) {
	eqm <- solve_model_dt_2(payoff_grid)

	follower_payoff_grid_plot <- ggplot(payoff_grid %>% filter(h_L==eqm$L[1], Q_L==eqm$L[2]), aes(x=Q_F, y=h_F)) +
		theme(text = element_text(size=20, family="Arial")) +
		geom_tile(aes(fill=profit_F)) +
    	scale_fill_fermenter() +
		geom_point(aes(x=eqm$F[2], y=eqm$F[1]), size=3, color="firebrick4") +
		labs(title = "Follower payoff", y = "Follower altitude [km]", x="Follower size [sats]", fill="Profits") +
		theme_bw()

	follower_payoff_grid_plot_L_choices <- ggplot(payoff_grid %>% filter(h_F==eqm$F[1], Q_F==eqm$F[2]), aes(x=Q_L, y=h_L)) +
		theme(text = element_text(size=20, family="Arial")) +
		geom_tile(aes(fill=profit_F)) +
    	scale_fill_fermenter() +
		geom_point(aes(x=eqm$L[2], y=eqm$L[1]), size=3, color="firebrick4") +
		labs(title = "Follower payoff", y = "Leader altitude [km]", x="Leader size [sats]", fill="Profits") +
		theme_bw()

	leader_payoff_grid_plot <- ggplot(payoff_grid %>% filter(h_F==eqm$F[1], Q_F==eqm$F[2]), aes(x=Q_L, y=h_L)) +
		theme(text = element_text(size=20, family="Arial")) +
		geom_tile(aes(fill=profit_L)) +
    	scale_fill_fermenter() +
		geom_point(aes(x=eqm$L[2], y=eqm$L[1]), size=3, color="firebrick4") +
		labs(title = "Leader payoff", y = "Leader altitude [km]", x="Leader size [sats]", fill="Profits") +
		theme_bw()

	leader_payoff_grid_plot_F_choices <- ggplot(payoff_grid %>% filter(h_L==eqm$L[1], Q_L==eqm$L[2]), aes(x=Q_F, y=h_F)) +
		theme(text = element_text(size=20, family="Arial")) +
		geom_tile(aes(fill=profit_L)) +
    	scale_fill_fermenter() +
		geom_point(aes(x=eqm$F[2], y=eqm$F[1]), size=3, color="firebrick4") +
		labs(title = "Leader payoff", y = "Follower altitude [km]", x="Follower size [sats]", fill="Profits") +
		theme_bw()

	return(list(
		follower_F=follower_payoff_grid_plot, 
		leader_L=leader_payoff_grid_plot,
		follower_L=follower_payoff_grid_plot_L_choices,
		leader_F=leader_payoff_grid_plot_F_choices
		))
}

# Function to plot best-response surfaces on quantity and height dimensions when both can choose both
plot_BRs_2 <- function(params, hQ2_grid, payoff_grid) {
	eqm <- solve_model_dt_2(payoff_grid)

	eqm_data <- eqm$full_data %>%
		group_by(L_designs) %>%
		mutate(
			F_BR_h = h_F[best_F_design_ind==TRUE],
			F_BR_Q = Q_F[best_F_design_ind==TRUE]
			) %>%
		ungroup() %>% 
		group_by(F_designs) %>%
		mutate(
			L_BR_h = h_L[best_L_design_ind==TRUE],
			L_BR_Q = Q_L[best_L_design_ind==TRUE]
			) %>%
		ungroup()

	F_h_data <- eqm_data %>% select(h_L, Q_L, F_BR_h, best_F_design_ind) %>% filter(best_F_design_ind==TRUE) %>% distinct()
	F_Q_data <- eqm_data %>% select(h_L, Q_L, F_BR_Q, best_F_design_ind) %>% filter(best_F_design_ind==TRUE) %>% distinct()
	L_h_data <- eqm_data %>% select(h_F, Q_F, L_BR_h, best_L_design_ind) %>% filter(best_L_design_ind==TRUE) %>% distinct()
	L_Q_data <- eqm_data %>% select(h_F, Q_F, L_BR_Q, best_L_design_ind) %>% filter(best_L_design_ind==TRUE) %>% distinct()

	follower_BR_h <- ggplot(F_h_data, aes(x=Q_L, y=h_L)) +
		theme(text = element_text(size=20)) +
		geom_tile(aes(fill=F_BR_h), alpha = 0.75) +
		geom_point(aes(x=eqm$L[2], y=eqm$L[1]), size=3, color="firebrick4") +
		labs(title = "Follower best-response", y = "Leader altitude [km]", x="Leader size [sats]", fill="Follower\naltitude [km]") +
		theme_bw()

	if(length(unique(F_h_data$F_BR_h))>1) {
		follower_BR_h <- follower_BR_h + scale_fill_fermenter() 
		}

	follower_BR_Q <- ggplot(F_Q_data, aes(x=Q_L, y=h_L)) +
		theme(text = element_text(size=20)) +
		geom_tile(aes(fill=F_BR_Q), alpha = 0.75) +
		geom_point(aes(x=eqm$L[2], y=eqm$L[1]), size=3, color="firebrick4") +
		labs(title = "Follower best-response", y = "Leader altitude [km]", x="Leader size [sats]", fill="Follower\nsize [sats]") +
		theme_bw()

	if(length(unique(F_Q_data$F_BR_Q))>1) {
		follower_BR_Q <- follower_BR_Q + scale_fill_fermenter() 
		}

	leader_BR_h <- ggplot(L_h_data, aes(x=Q_F, y=h_F)) +
		theme(text = element_text(size=20)) +
		geom_tile(aes(fill=L_BR_h), alpha = 0.75) +
		geom_point(aes(x=eqm$F[2], y=eqm$F[1]), size=3, color="firebrick4") +
		labs(title = "Leader best-response", y = "Follower altitude [km]", x="Follower size [sats]", fill="Leader\naltitude [km]") +
		theme_bw()

	if(length(unique(L_h_data$L_BR_h))>1) {
		leader_BR_h <- leader_BR_h + scale_fill_fermenter() 
		}

	leader_BR_Q <- ggplot(L_Q_data, aes(x=Q_F, y=h_F)) +
		theme(text = element_text(size=20)) +
		geom_tile(aes(fill=L_BR_Q), alpha = 0.75) +
		geom_point(aes(x=eqm$F[2], y=eqm$F[1]), size=3, color="firebrick4") +
		labs(title = "Leader best-response", y = "Follower altitude [km]", x="Follower size [sats]", fill="Leader\nsize [sats]") +
		theme_bw()

	if(length(unique(L_Q_data$L_BR_Q))>1) {
		leader_BR_Q <- leader_BR_Q + scale_fill_fermenter() 
		}

	return(list(
		follower_h=follower_BR_h, 
		leader_h=follower_BR_Q,
		follower_Q=leader_BR_h,
		leader_Q=leader_BR_Q
		))
}

#####
# Data wrangling functions
#####

# Function to check which vector is longer, and pad the end with zeros so they're both the same length. 
vector_length_equalizer <- function(vec1, vec2) {
	if(length(vec1) > length(vec2)) {
		vec2 <- c(vec2, rep(0, length(vec1) - length(vec2)))
	} else if(length(vec1) < length(vec2)) {
		vec1 <- c(vec1, rep(0, length(vec2) - length(vec1)))
	}
	return(list(vec1, vec2))
}