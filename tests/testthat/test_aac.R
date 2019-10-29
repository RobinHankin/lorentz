## Tests of nullvector functionality

test_that("Test suite aac.R",{
  
  for(i in 1:2){

      expect_silent(v <- r3vel(5))
      expect_silent(as.4vel(v))
     
      expect_silent(vel_to_4mom(v))       
      expect_silent(vel_to_4mom(v,   1:5))
      expect_silent(vel_to_4mom(v[1],1:5))
      
      n <- 10
      s <- 0.9


      ## reset speed of light:
      sol(1)
  }
})
