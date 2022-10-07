//=========================================//
//     Check Join Dialog                   //
//=========================================//
// Day la To Van Linh dang test //

CHECKJOIN:dialog {
	label = "DAY LA CHUONG TRINH CHECK JOIN" ;
	: column {
		// ===== Choose Object =====
	
		: row {
			label = "Chon doi tuong ( Choose Object)" ;
			:toggle {
				label = "Arc" ;
				key = "ObjArc" ;
				value = "1" ;
				} // Finish Toggle
			:toggle {
				label = "Line" ;
				key = "ObjLine" ;
				value = "1" ;
				} // Finish Toggle
			:popup_list {
				label = "Chon sai so cho phep ( Choose tolerance )";
				key = "Tole" ;
				list = "Justify\n0.0\n0.00\n0.000\n0.0000\n0.00000\n0.000000" ;
				}

			} // End Row
		// ===== CHECK MINI ARC       =====
		: row {
			label = "Option More....";
			:toggle {
				label = "Check some min gap arc , Yes / No?";
				key = "Checkgap";
				value = "0" ;
				} // Finish Toggle
			:edit_box {
				label = "Enter value" ;
				key = "Checkgap_value" ;
				value = 0.1 ;
				}

			} // End Row
		// ===== RUN AND CANCEL BUTTON=====
		: row {
			: boxed_row {
				:button {
					label = "Chay di (Running ...";
					key = "accept" ;
					is_default = true ;
				}
				:button {
					label = "Huy bo ( Cancel )";
					key = "cancel" ;
					is_default = false ;
					is_cancel = true ;
				}
				

				} // end boxed_row

			} // End Row

			
		// =====INFORMATION======
		:boxed_row {  
			:boxed_column {
				label = "Thong tin lien he " ;
				:text {
					label = "Design by To Van Linh  " ;
					}
				:text {
					label = "Mobile : +8433 356 8236  " ;
					}
				:text {
					label = "@ Copyright by Mechanic Team - RD Filter  " ;
					}
				:text {
					label = "V-ACE - ACE Antena Co,. Ltd . All rights reserved." ;
					}
				} // end boxed_column
			} // End - Hang thong tin lien he 


		} // finish column

		} // Finish "Check Join" Dialog