#lang racket
(define map_2 (lambda (d_5) (hash-ref d_5 'map_2)))(define add_2 (lambda (d_20) (hash-ref d_20 'add_2)))
(define +_3 (lambda (d_11) (add_2 d_11)))
(define d_Add_14 (hash 'add_2 (lambda (x_21) (lambda (x_22) (+ x_21 x_22)))))
(define subtract_7 (lambda (d_23) (hash-ref d_23 'subtract_7)))
(define -_8 (lambda (d_16) (subtract_7 d_16)))
(define d_Subtract_19 (hash 'subtract_7
                            (lambda (x_24) (lambda (x_25) (- x_24 x_25)))))(define false_0 ((lambda (x_10) #f) '()))
(define true_1 ((lambda (x_11) #t) '()))
(define not_2 (lambda (x_3) (if x_3 
            false_0
            true_1)))
(define ?pipe??pipe?_4 (lambda (x_5) (lambda (y_6) (if x_5 
                true_1
                y_6))))
(define &&_7 (lambda (x_8) (lambda (y_9) (if x_8 
                y_9
                false_0))))(define eq_2 (lambda (d_21) (hash-ref d_21 'eq_2)))
(define ==_3 (lambda (d_12) (eq_2 d_12)))
(define /=_5 (lambda (d_16) (lambda (x_7) (lambda (y_8) (not_2 (((==_3 d_16) x_7) y_8))))))
(define d_Eq_19 (hash 'eq_2 (lambda (x_22) (lambda (x_23) (= x_22 x_23)))))
(define d_Eq_20 (hash 'eq_2
                      (lambda (x_9) (lambda (y_10) (if x_9 
                                  y_10
                                  (not_2 y_10))))))(define foldr_2 (lambda (d_6) (hash-ref d_6 'foldr_2)))
(define foldl_5 (lambda (d_7) (hash-ref d_7 'foldl_5)))(define flip_0 (lambda (f_4) (lambda (x_5) (lambda (y_6) ((f_4 y_6) x_5)))))(define :_4 (lambda (x_289) (lambda (x_290) (list 1 x_289 x_290))))
(define ++_6 (lambda (l_8) (lambda (ys_9) (let* [(j_291 (lambda () ys_9))] 
                (let* [(j_292 (lambda (x_10 xs_11) (((lambda (x_294) (lambda (x_295) (list 1
                                                                                           x_294
                                                                                           x_295))) x_10) ((++_6 xs_11) ys_9))))] 
                    (let* [(s_296 l_8)] 
                        (case (car s_296) ((0) (j_291 ))
                                          ((1) (let* [(c_297 (cadr s_296))
                                                      (c_298 (caddr s_296))] 
                                              (j_292 c_297 c_298))))))))))
(define uncons_12 (lambda (l_14) (let* [(j_299 (lambda () (list 0)))] 
            (let* [(j_301 (lambda (x_15 xs_16) ((lambda (x_303) (list 1
                                                                      x_303)) (((lambda (x_306) (lambda (x_307) (list 0
                                                                                                                      x_306
                                                                                                                      x_307))) x_15) xs_16))))] 
                (let* [(s_308 l_14)] 
                    (case (car s_308) ((0) (j_299 ))
                                      ((1) (let* [(c_309 (cadr s_308))
                                                  (c_310 (caddr s_308))] 
                                          (j_301 c_309 c_310)))))))))
(define null_17 (lambda (l_19) (let* [(j_311 (lambda () true_1))] 
            (let* [(j_312 (lambda () false_0))] 
                (let* [(s_313 l_19)] 
                    (case (car s_313) ((0) (j_311 )) (else (j_312 ))))))))
(define d_Foldable_130 (hash 'foldr_2
                             (lambda (f_20) (lambda (z_21) (lambda (l_22) (let* [(j_314 (lambda () z_21))] 
                                             (let* [(j_315 (lambda (x_23 xs_24) ((f_20 x_23) ((((foldr_2 d_Foldable_130) f_20) z_21) xs_24))))] 
                                                 (let* [(s_316 l_22)] 
                                                     (case (car s_316) ((0) (j_314 ))
                                                                       ((1) (let* [(c_317 (cadr s_316))
                                                                                   (c_318 (caddr s_316))] 
                                                                           (j_315 c_317
                                                                                  c_318))))))))))
                             'foldl_5
                             (lambda (f_25) (lambda (z_26) (lambda (l_27) (let* [(j_319 (lambda () z_26))] 
                                             (let* [(j_320 (lambda (x_28 xs_29) ((((foldl_5 d_Foldable_130) f_25) ((f_25 z_26) x_28)) xs_29)))] 
                                                 (let* [(s_321 l_27)] 
                                                     (case (car s_321) ((0) (j_319 ))
                                                                       ((1) (let* [(c_322 (cadr s_321))
                                                                                   (c_323 (caddr s_321))] 
                                                                           (j_320 c_322
                                                                                  c_323))))))))))))
(define length_30 (((foldr_2 d_Foldable_130) (lambda (__32) (lambda (r_33) (((+_3 d_Add_14) r_33) 1)))) 0))
(define d_Functor_154 (hash 'map_2
                            (lambda (f_34) (lambda (l_35) ((((foldr_2 d_Foldable_130) (lambda (x_36) (lambda (r_37) ((:_4 (f_34 x_36)) r_37)))) (list 0)) l_35)))))
(define reverse_38 (((foldl_5 d_Foldable_130) (flip_0 (lambda (x_326) (lambda (x_327) (list 1
                                                                                            x_326
                                                                                            x_327))))) (list 0)))
(define filter_40 (lambda (pred_42) (((foldr_2 d_Foldable_130) (lambda (x_43) (lambda (r_44) (if (pred_42 x_43) 
                                                                           ((:_4 x_43) r_44)
                                                                           r_44)))) (list 0))))
(define concat_45 (lambda (d_182) (((foldr_2 d_182) ++_6) (list 0))))
(define concatMap_48 (lambda (d_192) (lambda (f_52) (lambda (l_53) ((((foldr_2 d_192) (lambda (x_54) (lambda (r_55) ((++_6 (f_52 x_54)) r_55)))) (list 0)) l_53)))))
(define scanl_56 (lambda (f_59) (lambda (z_60) (lambda (l_61) (let* [(j_332 (lambda () ((:_4 z_60) (list 0))))] 
                    (let* [(j_334 (lambda (x_62 xs_63) ((:_4 z_60) (((scanl_56 f_59) ((f_59 z_60) x_62)) xs_63))))] 
                        (let* [(s_335 l_61)] 
                            (case (car s_335) ((0) (j_332 ))
                                              ((1) (let* [(c_336 (cadr s_335))
                                                          (c_337 (caddr s_335))] 
                                                  (j_334 c_336 c_337)))))))))))
(define scanl1_64 (lambda (f_66) (lambda (l_67) (let* [(j_338 (lambda () (list 0)))] 
                (let* [(j_340 (lambda (x_68 xs_69) (((scanl_56 f_66) x_68) xs_69)))] 
                    (let* [(s_341 l_67)] 
                        (case (car s_341) ((0) (j_338 ))
                                          ((1) (let* [(c_342 (cadr s_341))
                                                      (c_343 (caddr s_341))] 
                                              (j_340 c_342 c_343))))))))))
(define replicate_70 (lambda (n_72) (lambda (x_73) (if (((<=_7 d_Ord_45) n_72) 0) 
                (list 0)
                ((:_4 x_73) ((replicate_70 (((-_8 d_Subtract_19) n_72) 1)) x_73))))))
(define unfoldr_74 (lambda (f_77) (lambda (z_78) (let* [(j_345 (lambda () (list 0)))] 
                (let* [(j_347 (lambda (x_79 r_80) ((:_4 x_79) ((unfoldr_74 f_77) r_80))))] 
                    (let* [(s_348 (f_77 z_78))] 
                        (case (car s_348) ((0) (j_345 ))
                                          ((1) (let* [(c_349 (cadr s_348))] 
                                              (case (car c_349) ((0) (let* [(c_350 (cadr c_349))
                                                                            (c_351 (caddr c_349))] 
                                                                    (j_347 c_350
                                                                           c_351)))))))))))))
(define take_81 (lambda (n_83) (lambda (xs_84) (if (((<=_7 d_Ord_45) n_83) 0) 
                (list 0)
                (let* [(j_353 (lambda () (list 0)))] 
                    (let* [(j_355 (lambda (x_85 xs_86) ((:_4 x_85) ((take_81 (((-_8 d_Subtract_19) n_83) 1)) xs_86))))] 
                        (let* [(s_356 xs_84)] 
                            (case (car s_356) ((0) (j_353 ))
                                              ((1) (let* [(c_357 (cadr s_356))
                                                          (c_358 (caddr s_356))] 
                                                  (j_355 c_357 c_358)))))))))))
(define drop_87 (lambda (n_89) (lambda (xs_90) (if (((<=_7 d_Ord_45) n_89) 0) 
                xs_90
                (let* [(j_359 (lambda () (list 0)))] 
                    (let* [(j_361 (lambda (x_91 xs_92) ((drop_87 (((-_8 d_Subtract_19) n_89) 1)) xs_92)))] 
                        (let* [(s_362 xs_90)] 
                            (case (car s_362) ((0) (j_359 ))
                                              ((1) (let* [(c_363 (cadr s_362))
                                                          (c_364 (caddr s_362))] 
                                                  (j_361 c_363 c_364)))))))))))
(define partition_acc_93 (lambda (acc_95) (lambda (pred_96) (lambda (l_97) (let* [(j_365 (lambda () acc_95))] 
                    (let* [(j_366 (lambda (x_98 xs_99) (let* [(j_367 (lambda (pass_100 fail_101) (if (pred_96 x_98) 
                                                                          (((partition_acc_93 (((lambda (x_370) (lambda (x_371) (list 0
                                                                                                                                      x_370
                                                                                                                                      x_371))) ((:_4 x_98) pass_100)) fail_101)) pred_96) xs_99)
                                                                          (((partition_acc_93 (((lambda (x_374) (lambda (x_375) (list 0
                                                                                                                                      x_374
                                                                                                                                      x_375))) pass_100) ((:_4 x_98) fail_101))) pred_96) xs_99))))] 
                                       (let* [(s_376 acc_95)] 
                                           (case (car s_376) ((0) (let* [(c_377 (cadr s_376))
                                                                         (c_378 (caddr s_376))] 
                                                                 (j_367 c_377
                                                                        c_378))))))))] 
                        (let* [(s_379 l_97)] 
                            (case (car s_379) ((0) (j_365 ))
                                              ((1) (let* [(c_380 (cadr s_379))
                                                          (c_381 (caddr s_379))] 
                                                  (j_366 c_380 c_381)))))))))))
(define partition_102 (partition_acc_93 (((lambda (x_384) (lambda (x_385) (list 0
                                                                                x_384
                                                                                x_385))) (list 0)) (list 0))))
(define sortBy_104 (lambda (comp_106) (lambda (l_107) (let* [(j_388 (lambda () (list 0)))] 
                (let* [(j_390 (lambda (x_108 xs_109) (let* [(j_391 (lambda (smaller_110 larger_111) ((++_6 ((sortBy_104 comp_106) smaller_110)) ((:_4 x_108) ((sortBy_104 comp_106) larger_111)))))] 
                                   (let* [(s_392 ((partition_102 ((geBy_24 comp_106) x_108)) xs_109))] 
                                       (case (car s_392) ((0) (let* [(c_393 (cadr s_392))
                                                                     (c_394 (caddr s_392))] 
                                                             (j_391 c_393
                                                                    c_394))))))))] 
                    (let* [(s_395 l_107)] 
                        (case (car s_395) ((0) (j_388 ))
                                          ((1) (let* [(c_396 (cadr s_395))
                                                      (c_397 (caddr s_395))] 
                                              (j_390 c_396 c_397))))))))))
(define sort_112 (lambda (d_284) (sortBy_104 (compare_6 d_284))))(define compare_6 (lambda (d_46) (hash-ref d_46 'compare_6)))
(define <=_7 (lambda (d_32) (lambda (x_9) (lambda (y_10) (let* [(j_47 (lambda () true_1))] 
                    (let* [(j_48 (lambda () false_0))] 
                        (let* [(s_49 (((compare_6 d_32) x_9) y_10))] 
                            (case (car s_49) ((0) (j_47 ))
                                             ((1) (j_47 ))
                                             (else (j_48 ))))))))))
(define <_11 (lambda (d_36) (lambda (x_13) (lambda (y_14) (let* [(j_50 (lambda () true_1))] 
                    (let* [(j_51 (lambda () false_0))] 
                        (let* [(s_52 (((compare_6 d_36) x_13) y_14))] 
                            (case (car s_52) ((0) (j_50 ))
                                             (else (j_51 ))))))))))
(define >_15 (lambda (d_40) (lambda (x_17) (lambda (y_18) (let* [(j_53 (lambda () true_1))] 
                    (let* [(j_54 (lambda () false_0))] 
                        (let* [(s_55 (((compare_6 d_40) x_17) y_18))] 
                            (case (car s_55) ((2) (j_53 ))
                                             (else (j_54 ))))))))))
(define leBy_19 (lambda (comp_21) (lambda (x_22) (lambda (y_23) (let* [(j_56 (lambda () true_1))] 
                    (let* [(j_57 (lambda () true_1))] 
                        (let* [(j_58 (lambda () false_0))] 
                            (let* [(s_59 ((comp_21 x_22) y_23))] 
                                (case (car s_59) ((0) (j_56 ))
                                                 ((1) (j_57 ))
                                                 ((2) (j_58 )))))))))))
(define geBy_24 (lambda (comp_26) (lambda (x_27) (lambda (y_28) (let* [(j_60 (lambda () false_0))] 
                    (let* [(j_61 (lambda () true_1))] 
                        (let* [(j_62 (lambda () true_1))] 
                            (let* [(s_63 ((comp_26 x_27) y_28))] 
                                (case (car s_63) ((0) (j_60 ))
                                                 ((1) (j_61 ))
                                                 ((2) (j_62 )))))))))))
(define d_Ord_45 (hash 'compare_6
                       (lambda (x_29) (lambda (y_30) (if (((lambda (x_64) (lambda (x_65) (<= x_64
                                                                                             x_65))) x_29) y_30) 
                                   (list 0)
                                   (if (((lambda (x_66) (lambda (x_67) (= x_66
                                                                          x_67))) x_29) y_30) 
                                       (list 1)
                                       (list 2)))))))(define plus_2 (lambda (d_12) (hash-ref d_12 'plus_2)))
(define +_3 (lambda (d_8) (plus_2 d_8)))
(define d_Add_11 (hash 'plus_2
                       (lambda (x_5) (lambda (y_6) (((lambda (x_13) (lambda (x_14) (+ x_13
                                                                                      x_14))) x_5) y_6)))))(define fst_22 (lambda (t_25) (let* [(j_42 (lambda (a_26 b_27) a_26))] 
            (let* [(s_43 t_25)] 
                (case (car s_43) ((0) (let* [(c_44 (cadr s_43))
                                             (c_45 (caddr s_43))] 
                                     (j_42 c_44 c_45))))))))
(define snd_28 (lambda (t_31) (let* [(j_46 (lambda (a_32 b_33) b_33))] 
            (let* [(s_47 t_31)] 
                (case (car s_47) ((0) (let* [(c_48 (cadr s_47))
                                             (c_49 (caddr s_47))] 
                                     (j_46 c_48 c_49))))))))