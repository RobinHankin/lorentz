## Tests of nullvector functionality

test_that("Test suite aac.R",{
  
  for(i in 1:2){

      expect_silent(v <- r3vel(5))
      expect_silent(as.4vel(v))
     
      expect_silent(vel_to_4mom(v))       
      expect_silent(vel_to_4mom(v,   1:5))
      expect_silent(vel_to_4mom(v[1],1:5))

      expect_silent(p_to_4mom(v,E=1))


      expect_silent(cosines(v))
   
      P <- vel_to_4mom(as.3vel(c(0.8,0,0)) + r3vel(7,0.01))
      expect_true(is.4mom(P))    
      expect_output(print(P))
      expect_silent(+P)
      expect_silent(-P)
      expect_error(!P)
      expect_error(P&P)
      expect_error(P+1)
      expect_error(1+P)
      expect_error(P*P)

      expect_silent(P)
      expect_silent(P*2)
      expect_silent(2*P)
      expect_silent(P+P)

    
      expect_silent(as.photon(v))
      expect_silent(as.photon(1:3))
      expect_silent(as.photon(v,E=3))

      expect_true(all(is.consistent.nullvec(as.photon(v))))
      expect_error(as.photon(function(x){x^2}))
     
      expect_true(is.4mom(reflect(P)))
      expect_true(is.4mom(reflect(P,c(1,0,0))))
      expect_true(is.4mom(reflect(P,c(0,1,0))))
      expect_true(is.4mom(reflect(P,c(0,0,1))))

      expect_true(all((reflect(reflect(P))-P)[,-1]==0))

      expect_silent(sum(P))

      ## reset speed of light:
      sol(1)
  }
})
