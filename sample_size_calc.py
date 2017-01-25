# -*- coding: utf-8 -*-
import numpy as np


def sample_size_calculator(z_score, stdev, margin_of_error, hit_price=False):
	"""
	Computes sample size required for specified statistical power. 
	Even outputs how much it would cost on MechTurk. 
	Based on http://success.qualtrics.com/rs/qualtrics/images/Determining-Sample-Size.pdf
	Formula:  Necessary Sample Size = (Z-score)² * StdDev*(1-StdDev) / (margin of error)².
	Margin of error a.k.a. confint. 
	90% – Z Score = 1.645
	95% – Z Score = 1.96
	99% – Z Score = 2.326
	"""
	numerator= (float(z_score)*z_score)*(float(stdev)*(1-stdev))
	print "numi", numerator
	denominator= float(margin_of_error)*margin_of_error
	print "demi", denominator
	if hit_price:
		print "With one HIT costing $ {}, this adds up to $ {}, comes to {} after adding MechTurk fees. Very expensive.".format(
		hit_price, np.ceil(numerator / denominator)*float(hit_price), np.ceil(numerator / denominator)*float(hit_price)+((np.ceil(numerator / denominator)*float(hit_price)/100)*20))
	return (numerator / denominator)


x=sample_size_calculator(1.96, .5,.05, hit_price=.20)
print x