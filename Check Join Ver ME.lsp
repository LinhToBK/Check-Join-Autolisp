; Day la chuong trinh check xem join kin hay khong
; Design : To Van Linh - Mechanical Team - RF Department
; Version 1 : 28-Sep-2022
; Day la chuong trinh Linh dang update

;====================================================================
;=========           FUNCTION : MAIN()        
;====================================================================
(defun C:Checkjoin(/)

  
  ;--------------------------------------------------------
  ; .........Step 1 : Open DCL Diaglog                 
  ;--------------------------------------------------------
  (setq list_Data_DCL (Open_CheckJoin_DCL) ) ;

  
  ;--------------------------------------------------------
  ; Step 2 :  Load Data from DCL Dialog
  ;--------------------------------------------------------
  (setq status_arc (nth 0 list_Data_DCL)
	status_line (nth 1 list_Data_DCL)
	status_tolerance (nth 2 list_Data_DCL)
	status_checkgap (nth 3 list_Data_DCL)
	checkgap_value (nth 4 list_Data_DCL)
	) ;
    (if ( and (= status_arc 0) (= status_line 0))
    (progn
    	(alert "Khong chon doi tuong nao a ? ")
        (exit)
      ) ; end progn
    ) ; end if

  
  ;--------------------------------------------------------
  ; Step 3 :  Get position of point                        
  ;--------------------------------------------------------
    (setq linhlist (ssget)
	) ;
  (setq soluong (sslength linhlist)
	sothutu 0
	) ;
  (setq list_arc (list)
	list_line (list)
	) ; 
  (repeat soluong
    (progn
      (setq Object (ssname linhlist sothutu)
	    Object_value (entget Object)
	    Object_Name ( cdr (assoc 0 Object_value))
	    ) ;
      ;========= 
      (if (= Object_Name "ARC")
	(progn
		(setq goc_start (cdr (assoc 50 Object_value))
		      goc_end  (cdr (assoc 51 Object_value))
		      diem_tam (cdr (assoc 10 Object_value))
		      ban_kinh (cdr (assoc 40 Object_value))
		      point_start (polar diem_tam goc_start ban_kinh)
		      point_end (polar diem_tam goc_end ban_kinh)
		      ) ;
	  	(setq list_arc (append list_arc (list point_start))
		      list_arc (append list_arc (list point_end))
		      ) ;
	  	(if (= status_checkgap 1)
		  	(progn
			  (setq gap_value (distance point_end point_start))
			  (print (strcat "gap value la :" (rtos gap_value)))
			  (if (> checkgap_value gap_value)
			    (command "text" point_start 1.5 "0" "Diem nguy hiem" "" "")
			    ) ; end if 
			  ) ; end progn
		  ) ; end if
	  	
	  ) ; end progn
	) ; end if
      (if (= Object_Name "LINE")
	(progn
		(setq point_start (cdr (assoc 10 Object_value))
		      point_end (cdr (assoc 11 Object_value))
		      ) ;
	  	(setq list_line (append list_line (list point_start))
		      list_line (append list_line (list point_end))
		      ) ; 
	  ) ; end progn
	) ; end if 
      (setq sothutu (+ 1 sothutu))
      ) ; end progn
    ) ; end repeat
 
  
  ;--------------------------------------------------------
  ;           Step 4 :  Save data in list                  
  ;--------------------------------------------------------
; Neu ngay xu buoc di nhanh qua con duong mua
  (setq list_point (list))
  (if ( and (= status_arc 1) (= status_line 1))
    (setq list_point (append list_line list_arc))
    ) ; end if
  (if ( and (= status_arc 0) (= status_line 1))
    (setq list_point list_line)
    ) ; end if
  (if ( and (= status_arc 1) (= status_line 0))
    (setq list_point list_arc)
    ) ; end if
  

  
  ;--------------------------------------------------------
  ; Step 5 :  Check point in list                          
  ;--------------------------------------------------------
  ; if 2 point same is OKk
  ; if 1 point  is copy to list 1
  ; if > 2 point is copy to list 2
  (setq soluong (length list_point)
	);
  ;(print "So luong diem la : ")
  ;(print soluong)
  (setq point_NG (list))
  (setq dem_i 0 )

  ;(print "Day la list diem")
  ;(print list_point)
  

  ;(print "11111111111111")
  (repeat soluong
    ;(alert "Chay luot 1")
    (setq dem_j 0)
    (progn
      (setq dem_trung 0)
      (repeat soluong
	(progn
	    (setq first_point (nth dem_i list_point)
		  check_point (nth dem_j list_point)
		  khoang_cach (distance first_point check_point)
		  ) ;
	  (if (< khoang_cach 1e-10)
	    (setq dem_trung (+ 1 dem_trung))) ; endif
	  ) ; end progn
	(setq dem_j (+ 1 dem_j))
	) ; end repeat dem_j
;;;      (print "Day la dem trung")
;;;      (print dem_trung)
      (if (/= dem_trung 2) ; ton tai nhieu hon hoac it hon
	(progn
		(setq point_NG (append point_NG (list (nth dem_i list_point))))
	  	;(alert "Tim thay diem trung nhau")
	  )
	) ; end if 
      ) ; end progn
    (setq dem_i (+ 1 dem_i))
    ) ; end repeat dem i
  (print "Day la nhung diem NG")
  (print point_NG)


  ; Step 6 :  drawing circle to show point
  (setq quantity_point (length point_NG)
	dem_i 0)
(setq oos1 (getvar "osmode"))
(setvar "osmode" 0)
  (if (/= quantity_point 0)
    (progn
	  (repeat quantity_point
	    (setq draw_point (nth dem_i point_NG))
	    (command ".circle" draw_point 1.0  "")
	    (setq dem_i (+ 1 dem_i))
	    ) ; end repeat
      
      (alert (strcat "Chay xong roi nhe!!! Co : " (itoa quantity_point) " diem"))
      )
    (alert "Khong co diem trung nhau ( Close Curve )")
    ) ; end if
(setvar "osmode" oos1)
(princ)

  ) ; finish defun

;====================================================================
;=========           FUNCTION : Open_CheckJoin_DCL            =======
;====================================================================
(defun Open_CheckJoin_DCL()
  
  (setq dcl_id (LOAD_DIALOG "CheckJoinVersionMechanic.DCL"))
  (if (not (new_dialog "CHECKJOIN" dcl_id))
    (exit)
    ) ; end if

  ;.... SET UP VALUE  .....

  (setq tog_arc 1
	tog_line 1
	plst_tole 0
	tog_checkgap 0
	edbx_gap 0.1
  	) ; end setq

; ...... ACTION TITLE .......
(ACTION_TILE "ObjArc" "(setq tog_arc (atoi $value))")
(ACTION_TILE "ObjLine" "(setq tog_line (atoi $value))")
(ACTION_TILE "Tole" "(setq plst_tole (atoi $value))")
(ACTION_TILE "Checkgap" "(setq tog_checkgap (atoi $value))")
(ACTION_TILE "Checkgap_value" "(setq edbx_gap $value ) ")

; .......RUN DIALOG .........
(ACTION_TILE "cancel" "(DONE_DIALOG 0)")
(ACTION_TILE "accept" "(DONE_DIALOG 1)")

(setq status (START_DIALOG))
(UNLOAD_DIALOG dcl_id)

; ...... SAVE variables .....
  (setq runlist (list))
(if (= status 1 )
  (progn
    (setq edbx_gap (atof edbx_gap))
    (setq runlist (append runlist (list tog_arc)))
    (setq runlist (append runlist (list tog_line)))
    (setq runlist (append runlist (list plst_tole)))
    (setq runlist (append runlist (list tog_checkgap)))
    (setq runlist (append runlist (list edbx_gap)))
    (print runlist)
    ) ; end progn
  ) ; end if
  (setq runlist runlist)
  ) ; Finish Defun "Open_CheckJoin_DCL"

