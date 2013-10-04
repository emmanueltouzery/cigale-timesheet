{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module EmailTests (runEmailTests) where

import Test.Hspec
import Test.HUnit

import Codec.Mbox
import Text.ParserCombinators.Parsec (parse)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime

import Str
import Util
import TestUtil

import Email

runEmailTests :: Spec
runEmailTests = do
	testEmail1
	testEmail2
	testMimeNonUtf
	testMimeMultibyte
	testMimeB64
	testParseEmailDate
	testParseMultipartBody
	testParseMultipartBodyTextPlainAttach
	testMultipartAlternative
	testMultipartRelated
	testMultipartProblem

testEmail1 :: Spec
testEmail1 = it "parses a simple email structure" $ do
	assertEqual "doesn't match" "after headers\n" (parseMultipartBody "\n\nThis is a multi-part message in MIME format.\nseparator\nContent-Type: text/plain\n\nfirstpart\nseparator\nContent-Type: text/html\n\nafter headers\nseparator--")

testEmail2 :: Spec
testEmail2 = it "parses a simple email structure" $ do
	assertEqual "doesn't match" "firstpart\n" (parseMultipartBody "\n\nseparator\nContent-Type: text/html\n\nfirstpart\nseparator\n\nContent-Type: text/plain\n\nafter headers\nseparator--\n")

testMimeNonUtf :: Spec
testMimeNonUtf = it "parses simple quoted printable" $
	assertEqual "doesn't match" "RE: FW: import datoteke - ki so šle skozi" (decodeMime "=?iso-8859-2?Q?RE:_FW:_import_datoteke_-_ki_so_=B9le_skozi?=")

testMimeMultibyte :: Spec
testMimeMultibyte = it "parses multibyte quoted printable" $
	assertEqual "doesn't match" "Arrières plans" (decodeMime "=?utf-8?Q?Arri=C3=A8res_plans?=")

testMimeB64 :: Spec
testMimeB64 = it "parses base64 email subject" $
	assertEqual "doesn't match" "Prevent Foreclosure & Save Your Home " (decodeMime "=?iso-8859-1?B?UHJldmVudCBGb3JlY2xvc3VyZSAmIFNhdmUgWW91ciBIb21lIA=?=")

testParseEmailDate :: Spec
testParseEmailDate = it "parses email date" $
	assertEqual "doesn't match" expected (parseEmailDate "Sep 27 20:46:35 2013")
		where
			expected = LocalTime (fromGregorian 2013 9 27) (TimeOfDay 20 46 (fromIntegral 35))

testParseMultipartBody :: Spec
testParseMultipartBody = it "parses multipart body" $ do
	let source = [strT|
			--047d7bfeb46643323d04e617c30c
			Content-Type: text/plain; charset=ISO-8859-2; format=flowed; delsp=yes
			Content-Transfer-Encoding: base64
			
			UG92YWJsamVuaSBzdGUgYmlsaSBuYSB0YSBkb2dvZGVrLg0KDQpOYXppdjogWm9ibmkgcmVudGdl
			BiBMYXJhDQpab2JuaSByZW50Z2VuIExhcmENCktkYWo6IHBldCA0LiBva3QgMjAxMyAwNzoxMCAt
			IDA4OjEwIE9zcmVkbmppIGV2cm9wc2tpIOhhcyAtIEJlb2dyYWQNCktqZTogWkQgVmnoDQpLb2xl
			ZGFyOiBldG91emVyeUBnbWFpbC5jb20NCktkbzoNCiAgICAgKiBTaW1vbmEgSHZhbGnoIFRvdXpl
			CnktIG9yZ2FuaXphdG9yDQogICAgICogRW1tYW51ZWwgVG91emVyeQ0KDQpQb2Ryb2Jub3N0aSBk
			B2dvZGthOiAgDQpodHRwczovL3d3dy5nb29nbGUuY29tL2NhbGVuZGFyL2V2ZW50P2FjdGlvbj1W
			SUVXJmVpZD1aR3R4T1dWck1tazRNbkp0Y21SelozUmtkVEF5Tm1aek16QWdaWFJ2ZFhwbGNubEFi
			USZ0b2s9TXpFamMybHRiMjVoTG1oMllXeHBZeTUwYjNWNlpYSjVRR2R0WVdsc0xtTnZiV1JsT0Rn
			D1pUWm1Oemd5T1RobFltVTNPREl3TVdSaVlqVTBNR0U1TVRjNE1UTmlZbVF6TmpjJmN0ej1FdXJv
			CGUvQmVsZ3JhZGUmaGw9c2wNCg0KVmFiaWxvIGl6IEdvb2dsZSBLb2xlZGFyamE6IGh0dHBzOi8v
			D3d3Lmdvb2dsZS5jb20vY2FsZW5kYXIvDQoNClRvIGUtcG+5dG8gcHJlamVtYXRlIG5hIHJh6HVu
			IGV0b3V6ZXJ5QGdtYWlsLmNvbSwga2VyIHN0ZSBuYXJv6GVuaSBuYSAgDQpwb3ZhYmlsYSB2IGtv
			BGVkYXJqdSBldG91emVyeUBnbWFpbC5jb20uDQoNCshlIG5lIL5lbGl0ZSB2ZeggcHJlamVtYXRp
			IG9idmVzdGlsLCBzZSBwcmlqYXZpdGUgdiAgDQpodHRwczovL3d3dy5nb29nbGUuY29tL2NhbGVu
			ZGFyLyBpbiBzcHJlbWVuaXRlIG5hc3Rhdml0dmUgb2J2ZXN0aWwgemEgdGEgIA0Ka29sZWRhci4N
			Cg==
			--047d7bfeb46643323d04e617c30c
			Content-Type: text/html; charset=ISO-8859-2
			Content-Transfer-Encoding: quoted-printable
			
			message body, but html
			--047d7bfeb46643323d04e617c30c
			Content-Type: text/calendar; charset=UTF-8; method=REQUEST
			Content-Transfer-Encoding: quoted-printable
			
			BEGIN:VCALENDAR
			calendar contents
			END:VCALENDAR
			
			--047d7bfeb46643323d04e617c30c--
			--047d7bfeb46643324304e617c30e
			Content-Type: application/ics; name="invite.ics"
			Content-Disposition: attachment; filename="invite.ics"
			Content-Transfer-Encoding: base64
			
			QkVHSU46VkNBTEVOREFSDQpQUk9ESUQ6LS8vR29vZ2xlIEluYy8vR29vZ2xlIENhbGVuZGFyIDcw
			LjkwNTQvL0VODQpWRVJTSU9OOjIuMA0KQ0FMU0NBTEU6R1JFR09SSUFODQpNRVRIT0Q6UkVRVUVT
			VA0KQkVHSU46VkVWRU5UDQpEVFNUQVJUOjIwMTMxMDA0VDA1MTAwMFoNCkRURU5EOjIwMTMxMDA0
			VDA2MTAwMFoNCkRUU1RBTVA6MjAxMzA5MTFUMDg1NDAxWg0KT1JHQU5JWkVSOm1haWx0bzpzaW1v
			BmEuaHZhbGljLnRvdXplcnlAZ21haWwuY29tDQpVSUQ6ZGtxOWVrMmk4MnJtcmRzZ3RkdTAyNmZz
			MzBAZ29vZ2xlLmNvbQ0KQVRURU5ERUU7Q1VUWVBFPUlORElWSURVQUw7Uk9MRT1SRVEtUEFSVElD
			SVBBTlQ7UEFSVFNUQVQ9QUNDRVBURUQ7UlNWUD1UUlVFDQogO1gtTlVNLUdVRVNUUz0wOm1haWx0
			BzpzaW1vbmEuaHZhbGljLnRvdXplcnlAZ21haWwuY29tDQpBVFRFTkRFRTtDVVRZUEU9SU5ESVZJ
			RFVBTDtST0xFPVJFUS1QQVJUSUNJUEFOVDtQQVJUU1RBVD1ORUVEUy1BQ1RJT047UlNWUD0NCiBU
			UlVFO0NOPUVtbWFudWVsIFRvdXplcnk7WC1OVU0tR1VFU1RTPTA6bWFpbHRvOmV0b3V6ZXJ5QGdt
			YWlsLmNvbQ0KQ1JFQVRFRDoyMDEzMDkxMVQwODU0MDBaDQpERVNDUklQVElPTjpab2JuaSByZW50
			Z2VuIExhcmFcbk9nbGVqdGUgc2kgZG9nb2RlayBuYSBodHRwOi8vd3d3Lmdvb2dsZS5jb20NCiAv
			Y2FsZW5kYXIvZXZlbnQ/YWN0aW9uPVZJRVcmZWlkPVpHdHhPV1ZyTW1rNE1uSnRjbVJ6WjNSa2RU
			QXlObVp6TXpBZ1pYUnZkWHANCiBsY25sQWJRJnRvaz1NekVqYzJsdGIyNWhMbWgyWVd4cFl5NTBi
			M1Y2WlhKNVFHZHRZV2xzTG1OdmJXUmxPRGd3WlRabU56Z3lPVGgNCiBsWW1VM09ESXdNV1JpWWpV
			ME1HRTVNVGM0TVROaVltUXpOamMmY3R6PUV1cm9wZS9CZWxncmFkZSZobD1zbC4NCkxBU1QtTU9E
			SUZJRUQ6MjAxMzA5MTFUMDg1NDAwWg0KTE9DQVRJT046WkQgVmnEjQ0KU0VRVUVOQ0U6MA0KU1RB
			VFVTOkNPTkZJUk1FRA0KU1VNTUFSWTpab2JuaSByZW50Z2VuIExhcmENClRSQU5TUDpPUEFRVUUN
			CkVORDpWRVZFTlQNCkVORDpWQ0FMRU5EQVINCg==
			--047d7bfeb46643324304e617c30e--
			|]
	assertEqual "doesn't match" "message body, but html" (parseMultipartBody source) 

testParseMultipartBodyTextPlainAttach :: Spec
testParseMultipartBodyTextPlainAttach = it "parse multipart body text/plain plus attach" $ do
	let source = [strT|
			--047d7bfeb46643323d04e617c30c
			Content-Type: text/html;
                           charset=ISO-8859-2; format=flowed; delsp=yes
			Content-Transfer-Encoding: base64
			
			UG92YWJsamVuaSBzdGUgYmlsaSBuYSB0YSBkb2dvZGVrLg0KDQpOYXppdjogWm9ibmkgcmVudGdl
			BiBMYXJhDQpab2JuaSByZW50Z2VuIExhcmENCktkYWo6IHBldCA0LiBva3QgMjAxMyAwNzoxMCAt
			IDA4OjEwIE9zcmVkbmppIGV2cm9wc2tpIOhhcyAtIEJlb2dyYWQNCktqZTogWkQgVmnoDQpLb2xl
			ZGFyOiBldG91emVyeUBnbWFpbC5jb20NCktkbzoNCiAgICAgKiBTaW1vbmEgSHZhbGnoIFRvdXpl
			CnktIG9yZ2FuaXphdG9yDQogICAgICogRW1tYW51ZWwgVG91emVyeQ0KDQpQb2Ryb2Jub3N0aSBk
			B2dvZGthOiAgDQpodHRwczovL3d3dy5nb29nbGUuY29tL2NhbGVuZGFyL2V2ZW50P2FjdGlvbj1W
			SUVXJmVpZD1aR3R4T1dWck1tazRNbkp0Y21SelozUmtkVEF5Tm1aek16QWdaWFJ2ZFhwbGNubEFi
			USZ0b2s9TXpFamMybHRiMjVoTG1oMllXeHBZeTUwYjNWNlpYSjVRR2R0WVdsc0xtTnZiV1JsT0Rn
			D1pUWm1Oemd5T1RobFltVTNPREl3TVdSaVlqVTBNR0U1TVRjNE1UTmlZbVF6TmpjJmN0ej1FdXJv
			CGUvQmVsZ3JhZGUmaGw9c2wNCg0KVmFiaWxvIGl6IEdvb2dsZSBLb2xlZGFyamE6IGh0dHBzOi8v
			D3d3Lmdvb2dsZS5jb20vY2FsZW5kYXIvDQoNClRvIGUtcG+5dG8gcHJlamVtYXRlIG5hIHJh6HVu
			IGV0b3V6ZXJ5QGdtYWlsLmNvbSwga2VyIHN0ZSBuYXJv6GVuaSBuYSAgDQpwb3ZhYmlsYSB2IGtv
			BGVkYXJqdSBldG91emVyeUBnbWFpbC5jb20uDQoNCshlIG5lIL5lbGl0ZSB2ZeggcHJlamVtYXRp
			IG9idmVzdGlsLCBzZSBwcmlqYXZpdGUgdiAgDQpodHRwczovL3d3dy5nb29nbGUuY29tL2NhbGVu
			ZGFyLyBpbiBzcHJlbWVuaXRlIG5hc3Rhdml0dmUgb2J2ZXN0aWwgemEgdGEgIA0Ka29sZWRhci4N
			Cg==
			--047d7bfeb46643323d04e617c30c
			Content-Type: text/plain; charset=ISO-8859-2
			Content-Transfer-Encoding: quoted-printable
			
			message body, but html
			--047d7bfeb46643323d04e617c30c
			Content-Type: text/calendar; charset=UTF-8; method=REQUEST
			Content-Transfer-Encoding: quoted-printable
			
			BEGIN:VCALENDAR
			calendar contents
			END:VCALENDAR
			
			--047d7bfeb46643323d04e617c30c--
			--047d7bfeb46643324304e617c30e
			Content-Type: application/ics; name="invite.ics"
			Content-Disposition: attachment; filename="invite.ics"
			Content-Transfer-Encoding: base64
			
			QkVHSU46VkNBTEVOREFSDQpQUk9ESUQ6LS8vR29vZ2xlIEluYy8vR29vZ2xlIENhbGVuZGFyIDcw
			LjkwNTQvL0VODQpWRVJTSU9OOjIuMA0KQ0FMU0NBTEU6R1JFR09SSUFODQpNRVRIT0Q6UkVRVUVT
			VA0KQkVHSU46VkVWRU5UDQpEVFNUQVJUOjIwMTMxMDA0VDA1MTAwMFoNCkRURU5EOjIwMTMxMDA0
			VDA2MTAwMFoNCkRUU1RBTVA6MjAxMzA5MTFUMDg1NDAxWg0KT1JHQU5JWkVSOm1haWx0bzpzaW1v
			BmEuaHZhbGljLnRvdXplcnlAZ21haWwuY29tDQpVSUQ6ZGtxOWVrMmk4MnJtcmRzZ3RkdTAyNmZz
			MzBAZ29vZ2xlLmNvbQ0KQVRURU5ERUU7Q1VUWVBFPUlORElWSURVQUw7Uk9MRT1SRVEtUEFSVElD
			SVBBTlQ7UEFSVFNUQVQ9QUNDRVBURUQ7UlNWUD1UUlVFDQogO1gtTlVNLUdVRVNUUz0wOm1haWx0
			BzpzaW1vbmEuaHZhbGljLnRvdXplcnlAZ21haWwuY29tDQpBVFRFTkRFRTtDVVRZUEU9SU5ESVZJ
			RFVBTDtST0xFPVJFUS1QQVJUSUNJUEFOVDtQQVJUU1RBVD1ORUVEUy1BQ1RJT047UlNWUD0NCiBU
			UlVFO0NOPUVtbWFudWVsIFRvdXplcnk7WC1OVU0tR1VFU1RTPTA6bWFpbHRvOmV0b3V6ZXJ5QGdt
			YWlsLmNvbQ0KQ1JFQVRFRDoyMDEzMDkxMVQwODU0MDBaDQpERVNDUklQVElPTjpab2JuaSByZW50
			Z2VuIExhcmFcbk9nbGVqdGUgc2kgZG9nb2RlayBuYSBodHRwOi8vd3d3Lmdvb2dsZS5jb20NCiAv
			Y2FsZW5kYXIvZXZlbnQ/YWN0aW9uPVZJRVcmZWlkPVpHdHhPV1ZyTW1rNE1uSnRjbVJ6WjNSa2RU
			QXlObVp6TXpBZ1pYUnZkWHANCiBsY25sQWJRJnRvaz1NekVqYzJsdGIyNWhMbWgyWVd4cFl5NTBi
			M1Y2WlhKNVFHZHRZV2xzTG1OdmJXUmxPRGd3WlRabU56Z3lPVGgNCiBsWW1VM09ESXdNV1JpWWpV
			ME1HRTVNVGM0TVROaVltUXpOamMmY3R6PUV1cm9wZS9CZWxncmFkZSZobD1zbC4NCkxBU1QtTU9E
			SUZJRUQ6MjAxMzA5MTFUMDg1NDAwWg0KTE9DQVRJT046WkQgVmnEjQ0KU0VRVUVOQ0U6MA0KU1RB
			VFVTOkNPTkZJUk1FRA0KU1VNTUFSWTpab2JuaSByZW50Z2VuIExhcmENClRSQU5TUDpPUEFRVUUN
			CkVORDpWRVZFTlQNCkVORDpWQ0FMRU5EQVINCg==
			--047d7bfeb46643324304e617c30e--
			|] 
	assertEqual "doesn't match" "UG92YWJsamVuaSBzdGUgYmlsaSBuYSB0YSBkb2dvZGVrLg0KDQpOYXppdjogWm9ibmkgcmVudGdl\nBiBMYXJhDQpab2JuaSByZW50Z2VuIExhcmENCktkYWo6IHBldCA0LiBva3QgMjAxMyAwNzoxMCAt\nIDA4OjEwIE9zcmVkbmppIGV2cm9wc2tpIOhhcyAtIEJlb2dyYWQNCktqZTogWkQgVmnoDQpLb2xl\nZGFyOiBldG91emVyeUBnbWFpbC5jb20NCktkbzoNCiAgICAgKiBTaW1vbmEgSHZhbGnoIFRvdXpl\nCnktIG9yZ2FuaXphdG9yDQogICAgICogRW1tYW51ZWwgVG91emVyeQ0KDQpQb2Ryb2Jub3N0aSBk\nB2dvZGthOiAgDQpodHRwczovL3d3dy5nb29nbGUuY29tL2NhbGVuZGFyL2V2ZW50P2FjdGlvbj1W\nSUVXJmVpZD1aR3R4T1dWck1tazRNbkp0Y21SelozUmtkVEF5Tm1aek16QWdaWFJ2ZFhwbGNubEFi\nUSZ0b2s9TXpFamMybHRiMjVoTG1oMllXeHBZeTUwYjNWNlpYSjVRR2R0WVdsc0xtTnZiV1JsT0Rn\nD1pUWm1Oemd5T1RobFltVTNPREl3TVdSaVlqVTBNR0U1TVRjNE1UTmlZbVF6TmpjJmN0ej1FdXJv\nCGUvQmVsZ3JhZGUmaGw9c2wNCg0KVmFiaWxvIGl6IEdvb2dsZSBLb2xlZGFyamE6IGh0dHBzOi8v\nD3d3Lmdvb2dsZS5jb20vY2FsZW5kYXIvDQoNClRvIGUtcG+5dG8gcHJlamVtYXRlIG5hIHJh6HVu\nIGV0b3V6ZXJ5QGdtYWlsLmNvbSwga2VyIHN0ZSBuYXJv6GVuaSBuYSAgDQpwb3ZhYmlsYSB2IGtv\nBGVkYXJqdSBldG91emVyeUBnbWFpbC5jb20uDQoNCshlIG5lIL5lbGl0ZSB2ZeggcHJlamVtYXRp\nIG9idmVzdGlsLCBzZSBwcmlqYXZpdGUgdiAgDQpodHRwczovL3d3dy5nb29nbGUuY29tL2NhbGVu\nZGFyLyBpbiBzcHJlbWVuaXRlIG5hc3Rhdml0dmUgb2J2ZXN0aWwgemEgdGEgIA0Ka29sZWRhci4N\nCg==\n" (parseMultipartBody source)

testMultipartAlternative :: Spec
testMultipartAlternative = it "parse multipart alternative body plus attach" $ do
	let source = [strT|
			This is a multi-part message in MIME format.
			--------------070503040503000700050104
			Content-Type: multipart/alternative;
			 boundary="------------000008060108030905000402"
			
			
			--------------000008060108030905000402
			Content-Type: text/plain; charset=ISO-8859-2; format=flowed
			Content-Transfer-Encoding: 8bit
			
			text version
			
			--------------000008060108030905000402
			Content-Type: text/html; charset=ISO-8859-2
			Content-Transfer-Encoding: 8bit
			
			html version
			
			--------------000008060108030905000402--
			
			--------------070503040503000700050104
			Content-Type: application/octet-stream;
			 name="9780133018004.pdf"
			Content-Transfer-Encoding: base64
			Content-Disposition: attachment;
			 filename="9780133018004.pdf"
			
			JVBERi0xLjYKJfv8/f4KMjgzIDAgb2JqCjw8Ci9SZXNvdXJjZXMgMTQ2OCAwIFIKL1BhcmVu
			dCA0IDAgUgovQ3JvcEJveCBbMTIuOTYwMDAgMC43MjAwMCA0NzMuMDQwMDAgNjU1IF0KL0Nv
			bnRlbnRzIFsxNDY3IDAgUiAxNDcwIDAgUiAxNDY5IDAgUiBdCi9Bbm5vdHMgWzE0NzEgMCBS
			IF0KL01lZGlhQm94IFswIDAgNDczLjA0MDAwIDY1NSBdCi9Sb3RhdGUgMAovVHlwZSAvUGFn
			ZQovQXJ0Qm94IFswIDcyIDQ2OCA2NTUuMjAwMDAgXQo+PgplbmRvYmoKMzAwIDAgb2JqCjw8
			Ci9SZXNvdXJjZXMgMTQ3MyAwIFIKL1BhcmVudCA0IDAgUgovQ3JvcEJveCBbMTIuOTYwMDAg
			MC43MjAwMCA0NzMuMDQwMDAgNjU1IF0KL0NvbnRlbnRzIFsxNDcyIDAgUiAxNDc1IDAgUiAx
			--------------070503040503000700050104--
			|] -- WARNING i truncated the base64!!
	assertEqual "doesn't match" "html version\n\n" (parseMultipartBody source)

testMultipartRelated :: Spec
testMultipartRelated = it "parse multipart related body plus attach" $ do
	let source = [strT|
			This is a multi-part message in MIME format.
			--------------080701080400090206090002
			Content-Type: text/plain; charset=UTF-8; format=flowed
			Content-Transfer-Encoding: 8bit
			
			text
			
			--------------080701080400090206090002
			Content-Type: multipart/related;
			 boundary="------------040602030008000703040204"
			
			
			--------------040602030008000703040204
			Content-Type: text/html; charset=UTF-8
			Content-Transfer-Encoding: 8bit
			
			html content
			<img id="Picture_x0020_2" src="cid:part20.08050508.06050608@lecip-its.com" alt="cid:image002.jpg@01CEAF06.B364DF00" border="0" height="562" width="601">
			
			--------------040602030008000703040204
			Content-Type: image/jpeg
			Content-Transfer-Encoding: base64
			Content-ID: <part20.08050508.06050608@lecip-its.com>
			
			/9j/4AAQSkZJRgABAQEAYABgAAD/2wBDAAoHBwgHBgoICAgLCgoLDhgQDg0NDh0VFhEYIx8l
			JCIfIiEmKzcvJik0KSEiMEExNDk7Pj4+JS5ESUM8SDc9Pjv/2wBDAQoLCw4NDhwQEBw7KCIo
			Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozv/wAAR
			CAIyAlkDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAA
			AgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkK
			FhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWG
			h4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl
			--------------040602030008000703040204--
			
			--------------080701080400090206090002--
			|] -- WARNING i truncated the base64!!!
	assertEqual "doesn't match" "html content\n<img id=\"Picture_x0020_2\" src=\"cid:part20.08050508.06050608@lecip-its.com\" alt=\"cid:image002.jpg@01CEAF06.B364DF00\" border=\"0\" height=\"562\" width=\"601\">\n\n" (parseMultipartBody source)

testMultipartProblem :: Spec
testMultipartProblem = it "parse multipart problem body" $ do
	let source = [strT|
			------=_Part_261520_1752112592.1379489229611
			Content-Type: multipart/alternative; 
				boundary="----=_Part_261521_1460566080.1379489229611"
			
			------=_Part_261521_1460566080.1379489229611
			Content-Type: text/plain; charset=utf-8
			Content-Transfer-Encoding: quoted-printable
			
			do tega najverjetneje pride ker gledate stare podatke ali zato ker stara en=
			ota oddaja. namre=C4=8D je Dejan pomotoma spremenil kode za alerte in event=
			e. Ko je to naredil, smo se zna=C5=A1li v polo=C5=BEaju v katerem stare eve=
			nte so imeli en format in nove en drug, kar je polo=C5=BEaj v katerem se NO=
			=C4=8CE=C5=A0 najti. v glavnem, imamo po mojem dve re=C5=A1itvi:=20
			
			------=_Part_261521_1460566080.1379489229611
			Content-Type: multipart/related; 
				boundary="----=_Part_261522_1996021350.1379489229611"
			
			------=_Part_261522_1996021350.1379489229611
			Content-Type: text/html; charset=utf-8
			Content-Transfer-Encoding: quoted-printable
			
			<html> smo se zna=C5=A1li v polo=C5=BEaju se NO=
			=C4=8CE=C5=A0 najti
			------=_Part_261522_1996021350.1379489229611
			Content-Type: image/png; name=ddfiieci.png
			Content-Disposition: attachment; filename=ddfiieci.png
			Content-Transfer-Encoding: base64
			Content-ID: <part1.05020003.09090100@lecip-its.com>
			
			iVBORw0KGgoAAAANSUhEUgAAA9MAAABeCAIAAACvqdCJAAAgAElEQVR4nO2dXaKjIAyFux634kpc
			iAtxHbO5efCHBM4JYG1rb8/3MlMvhhBCiEjL4yGEEEIIIYQQQgghhBBCCCGEEEIIIYQQQgghhBDi
			------=_Part_261522_1996021350.1379489229611--
			
			------=_Part_261521_1460566080.1379489229611--
			
			------=_Part_261520_1752112592.1379489229611
			Content-Type: text/x-java; name=Event-current.java
			Content-Disposition: attachment; filename=Event-current.java
			Content-Transfer-Encoding: base64
			
			cGFja2FnZSBjb20ubGVjaXAudHJhbnNtaXR0ZXI7CgppbXBvcnQgamF2YS51dGlsLkxpc3Q7Cgpp
			bXBvcnQgb3JnLmpzb24uSlNPTkFycmF5OwppbXBvcnQgb3JnLmpzb24uSlNPTkV4Y2VwdGlvbjsK
			------=_Part_261520_1752112592.1379489229611--
			|] -- TODO truncated base64!!
	assertEqual "doesn't match" "<html> smo se znašli v položaju se NOČEŠ najti" (parseMultipartBody source)
