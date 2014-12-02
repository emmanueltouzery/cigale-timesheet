{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module EmailTests (runEmailTests) where

import Test.Hspec
import Test.HUnit

import Codec.Mbox
import Text.ParserCombinators.Parsec (parse)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import Control.Applicative

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
	testEmailCarriageReturnInMiddleOfMultibyte
	testHeadersParse
	testCharsetFromContenType
	--testMboxMessage
	testMboxMessageQP
	testMboxMultipartMessage
	testDifferentMessage

testEmail1 :: Spec
testEmail1 = it "parses a simple email structure" $
	assertEqual "doesn't match" "after headers" (getMultipartBodyText "separator" "\n\nThis is a multi-part message in MIME format.\n--separator\nContent-Type: text/plain\n\nfirstpart\n--separator\nContent-Type: text/html\n\nafter headers\n--separator--")

testEmail2 :: Spec
testEmail2 = it "parses a simple email structure" $
	assertEqual "doesn't match" "firstpart" (getMultipartBodyText "separator" "\n\n--separator\nContent-Type: text/html\n\nfirstpart\n--separator\n\nContent-Type: text/plain\n\nafter headers\n--separator--\n")

testMimeNonUtf :: Spec
testMimeNonUtf = it "parses simple quoted printable" $
	assertEqual "doesn't match" "RE: FW: import datoteke - ki so šle skozi" (decodeMimeHeader"=?iso-8859-2?Q?RE:_FW:_import_datoteke_-_ki_so_=B9le_skozi?=")

testMimeMultibyte :: Spec
testMimeMultibyte = it "parses multibyte quoted printable" $ do
	assertEqual "doesn't match" "Arrières plans" (decodeMimeHeader"=?utf-8?Q?Arri=C3=A8res_plans?=")
	assertEqual "doesn't match" "nadgradnja testni strežnik in FK" (decodeMimeHeader"nadgradnja testni =?UTF-8?B?c3RyZcW+bmlrIGluIEZL?=")
	assertEqual "doesn't match" "a=?b=?c" (decodeMimeHeader"a=?b=?c")
	assertEqual "doesn't match" "test =?test?= test1" (decodeMimeHeader"test =?test?= test1")

testMimeB64 :: Spec
testMimeB64 = it "parses base64 email subject" $
	assertEqual "doesn't match" "Prevent Foreclosure & Save Your Home " (decodeMimeHeader"=?iso-8859-1?B?UHJldmVudCBGb3JlY2xvc3VyZSAmIFNhdmUgWW91ciBIb21lIA=?=")

testParseEmailDate :: Spec
testParseEmailDate = it "parses email date" $ do
	assertEqual "doesn't match" expected (parseEmailDate "Sep 27 20:46:35 2013")
	assertEqual "test zoned" expected (parseEmailDate "Fri, 27 Sep 2013 20:46:35 +0100")
	assertEqual "test zoned" expected (parseEmailDate "Fri Sep 27 20:46:35 2013")
	assertEqual "test extra space" expected1 (parseEmailDate "Mon Nov  3 07:54:09 2014")
		where
			expected = LocalTime (fromGregorian 2013 9 27) (TimeOfDay 20 46 35)
			expected1 = LocalTime (fromGregorian 2014 11 3) (TimeOfDay 7 54 9)

testParseMultipartBody :: Spec
testParseMultipartBody = it "parses multipart body" $ do
	let source = BL.pack $ T.unpack [strT|
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
	assertEqual "doesn't match" "message body, but html"
		(getMultipartBodyText "047d7bfeb46643323d04e617c30c" source) 

testParseMultipartBodyTextPlainAttach :: Spec
testParseMultipartBodyTextPlainAttach = it "parse multipart body text/plain plus attach" $ do
	let source = BL.pack $ T.unpack [strT|
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
	assertEqual "doesn't match" "UG92YWJsamVuaSBzdGUgYmlsaSBuYSB0YSBkb2dvZGVrLg0KDQpOYXppdjogWm9ibmkgcmVudGdl\nBiBMYXJhDQpab2JuaSByZW50Z2VuIExhcmENCktkYWo6IHBldCA0LiBva3QgMjAxMyAwNzoxMCAt\nIDA4OjEwIE9zcmVkbmppIGV2cm9wc2tpIOhhcyAtIEJlb2dyYWQNCktqZTogWkQgVmnoDQpLb2xl\nZGFyOiBldG91emVyeUBnbWFpbC5jb20NCktkbzoNCiAgICAgKiBTaW1vbmEgSHZhbGnoIFRvdXpl\nCnktIG9yZ2FuaXphdG9yDQogICAgICogRW1tYW51ZWwgVG91emVyeQ0KDQpQb2Ryb2Jub3N0aSBk\nB2dvZGthOiAgDQpodHRwczovL3d3dy5nb29nbGUuY29tL2NhbGVuZGFyL2V2ZW50P2FjdGlvbj1W\nSUVXJmVpZD1aR3R4T1dWck1tazRNbkp0Y21SelozUmtkVEF5Tm1aek16QWdaWFJ2ZFhwbGNubEFi\nUSZ0b2s9TXpFamMybHRiMjVoTG1oMllXeHBZeTUwYjNWNlpYSjVRR2R0WVdsc0xtTnZiV1JsT0Rn\nD1pUWm1Oemd5T1RobFltVTNPREl3TVdSaVlqVTBNR0U1TVRjNE1UTmlZbVF6TmpjJmN0ej1FdXJv\nCGUvQmVsZ3JhZGUmaGw9c2wNCg0KVmFiaWxvIGl6IEdvb2dsZSBLb2xlZGFyamE6IGh0dHBzOi8v\nD3d3Lmdvb2dsZS5jb20vY2FsZW5kYXIvDQoNClRvIGUtcG+5dG8gcHJlamVtYXRlIG5hIHJh6HVu\nIGV0b3V6ZXJ5QGdtYWlsLmNvbSwga2VyIHN0ZSBuYXJv6GVuaSBuYSAgDQpwb3ZhYmlsYSB2IGtv\nBGVkYXJqdSBldG91emVyeUBnbWFpbC5jb20uDQoNCshlIG5lIL5lbGl0ZSB2ZeggcHJlamVtYXRp\nIG9idmVzdGlsLCBzZSBwcmlqYXZpdGUgdiAgDQpodHRwczovL3d3dy5nb29nbGUuY29tL2NhbGVu\nZGFyLyBpbiBzcHJlbWVuaXRlIG5hc3Rhdml0dmUgb2J2ZXN0aWwgemEgdGEgIA0Ka29sZWRhci4N\nCg==" (getMultipartBodyText "047d7bfeb46643323d04e617c30c" source)

testMultipartAlternative :: Spec
testMultipartAlternative = it "parse multipart alternative body plus attach" $ do
	let source = BL.pack $ T.unpack [strT|
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
	assertEqual "doesn't match" "html version\n" (getMultipartBodyText "------------070503040503000700050104" source)

testMultipartRelated :: Spec
testMultipartRelated = it "parse multipart related body plus attach" $ do
	let source = BL.pack $ T.unpack [strT|
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
			<img id="Picture_x0020_2" src="cid:part20.08050508.06050608@test.com" alt="cid:image002.jpg@01CEAF06.B364DF00" border="0" height="562" width="601">
			
			--------------040602030008000703040204
			Content-Type: image/jpeg
			Content-Transfer-Encoding: base64
			Content-ID: <part20.08050508.06050608@test.com>
			
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
	assertEqual "doesn't match" "html content\n<img id=\"Picture_x0020_2\" src=\"cid:part20.08050508.06050608@test.com\" alt=\"cid:image002.jpg@01CEAF06.B364DF00\" border=\"0\" height=\"562\" width=\"601\">\n" (getMultipartBodyText "------------080701080400090206090002" source)

testMultipartProblem :: Spec
testMultipartProblem = it "parse multipart problem body" $ do
	let source = BL.pack $ T.unpack [strT|
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
			Content-ID: <part1.05020003.09090100@test.com>
			
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
	assertEqual "doesn't match" "<html> smo se znašli v položaju se NOČEŠ najti" (getMultipartBodyText "----=_Part_261520_1752112592.1379489229611" source)

testEmailCarriageReturnInMiddleOfMultibyte = it "parses email carriage return in middle of multibyte" $ do
	let source = BL.pack $ T.unpack [strT|
		This is a multi-part message in MIME format.
		
		-------------7934315f87642993d731c3ef7372f2b3
		Content-Type: text/plain;
			charset="UTF-8"
		Content-Transfer-Encoding: quoted-printable
		
		Pozdravljen,
		-------------7934315f87642993d731c3ef7372f2b3
		Content-Type: text/html;
			charset="utf-8"
		Content-Transfer-Encoding: quoted-printable
		
		 celo danes zjutraj=3F Nekaj je bilo v soboto 15:35, ki mu ni bilo v=C5=
		=A1e=C4=8D. =C5=A0e raziskujem.
		-------------7934315f87642993d731c3ef7372f2b3--
		|]
	assertEqual "doesn't match" " celo danes zjutraj? Nekaj je bilo v soboto 15:35, ki mu ni bilo všeč. Še raziskujem." (getMultipartBodyText "-----------7934315f87642993d731c3ef7372f2b3" source)

testHeadersParse :: Spec
testHeadersParse = it "parses headers" $ do
	let source = BL.pack $ T.unpack [strT|
			X-Spam-Level:
			X-Spam-Status: No, score=-3.054
				tests=[ALL_TRUSTED=-1,
				T_NOT_A_PERSON=-0.01]
			Received: from www.google.com ([127.0.0.1])
				by localhost (www.google.com [127.0.0.1])
			
			|]
	assertEqual "doesn't match" [("X-Spam-Level", ""),
		("X-Spam-Status", "No, score=-3.054 tests=[ALL_TRUSTED=-1, T_NOT_A_PERSON=-0.01]"),
		("Received",
		   "from www.google.com ([127.0.0.1]) by localhost (www.google.com [127.0.0.1])")] 
		(Util.parsecError readHeaders "testHeadersParse" source)

testCharsetFromContenType :: Spec
testCharsetFromContenType = it "parses charset for content type" $ do
	let source = "text/plain; charset=windows-1252; format=flowed"
	assertEqual "doesn't match" "windows-1252" (charsetFromContentType source)

-- not sure why this one doesn't go through...
testMboxMessage :: Spec
testMboxMessage = it "parses a message from start to end" $ do
	let source = BL.pack $ T.unpack [strT|
			From - Thu Oct 03 13:34:30 2013
			X-Mozilla-Status: 0001
			X-Mozilla-Status2: 00800000
			X-Mozilla-Keys:                                                                                 
			Message-ID: <524D5646.90908@test.com>
			Date: Thu, 03 Oct 2013 13:34:30 +0200
			From: Emmanuel <emmanuel@test.com>
			MIME-Version: 1.0
			To: 'a b' <a.b@test.com>
			Subject: test
			Content-Type: text/plain; charset=UTF-8; format=flowed
			Content-Transfer-Encoding: 8bit
			
			Živjo
			|]
	let msg = MboxMessage "sender" "Jan 23 06:00:05 2013" source "file" 0
	let expected = Email
		{
			date = LocalTime (fromGregorian 2013 1 23) (TimeOfDay 6 0 5),
			to = "'a b' <a.b@test.com>",
			cc = Nothing,
			subject = "test",
			contents = "Živjo\n<br/>"
		}
	assertEqual "doesn't match" expected (messageToEmail' msg)

testMboxMessageQP :: Spec
testMboxMessageQP = it "parses a quoted-printable plain text message from start to end" $ do
	let source = BL.pack $ T.unpack [strT|
			From - Thu Oct 03 13:34:30 2013
			X-Mozilla-Status: 0001
			X-Mozilla-Status2: 00800000
			X-Mozilla-Keys:                                                                                 
			Message-ID: <524D5646.90908@test.com>
			Date: Thu, 03 Oct 2013 13:34:30 +0200
			From: Emmanuel <emmanuel@test.com>
			MIME-Version: 1.0
			To: 'a b' <a.b@test.com>
			Subject: test
			Content-Type: text/plain; charset=UTF-8; format=flowed
			Content-Transfer-Encoding: quoted-printable
			
			=C5=BDivjo,

			Lep pozdrav,

			Emmanuel
			|]
	let msg = MboxMessage "sender" "Jan 23 06:00:05 2013" source "file" 0
	let expected = Email
		{
			date = LocalTime (fromGregorian 2013 10 3) (TimeOfDay 13 34 30),
			to = "'a b' <a.b@test.com>",
			cc = Nothing,
			subject = "test",
			contents = "Živjo,\n<br/>Lep pozdrav,\n<br/>Emmanuel\n<br/>"
		}
	assertEqual "doesn't match" expected (messageToEmail' msg)

testMboxMultipartMessage :: Spec
testMboxMultipartMessage = it "parses a multipart message from start to end" $ do
	let source = BL.pack $ T.unpack [strT|
			From - Thu Oct 03 10:18:41 2013
			X-Mozilla-Status: 0001
			X-Mozilla-Status2: 00800000
			X-Mozilla-Keys:                                                                                 
			Message-ID: <524D2860.2060103@test.com>
			Date: Thu, 03 Oct 2013 10:18:40 +0200
			From: a b <a.b@test.com>
			MIME-Version: 1.0
			To: c d <c@test.com>
			Subject: Re: FW: Test
			Content-Type: multipart/alternative;
			 boundary="------------090707030607080209050905"
			
			This is a multi-part message in MIME format.
			--------------090707030607080209050905
			Content-Type: text/plain; charset=ISO-8859-2; format=flowed
			Content-Transfer-Encoding: 8bit
			
			yes, sorry i was focused on these upgrades and let the mails go a bit.
			
			--------------090707030607080209050905
			Content-Type: multipart/related;
			 boundary="------------040305040006080906090803"
			
			
			--------------040305040006080906090803
			Content-Type: text/html; charset=ISO-8859-2
			Content-Transfer-Encoding: 8bit
			
			<html>contents HTML
			</html>
			
			--------------040305040006080906090803
			Content-Type: image/png
			Content-Transfer-Encoding: base64
			Content-ID: <part2.04000607.07010808@test.com>
			
			iVBORw0KGgoAAAANSUhEUgAABAgAAANtCAIAAACSZd4qAAAAAXNSR0IArs4c6QAA/8pJREFU
			eF7sfQeYZGWVduWcc3XOcUJPYoacg6iIoIIKoq66ZmV1XXHRNfAvqCiorGExrqxiWExrYAVB
			RU5ErkJggg==
			
			--------------040305040006080906090803
			Content-Type: image/png
			Content-Transfer-Encoding: base64
			Content-ID: <part4.00020808.07050000@test.com>
			
			iVBORw0KGgoAAAANSUhEUgAAB4AAAAQQCAIAAACMTtFPAAAAAXNSR0IArs4c6QAA/8pJREFU
			eF7snQdgFFX+xyedhISEQCD0LqEX6WAHBREUAQX1lLNXznaennJ6ooINORV74e+pIEUQDkEB
			CCCAAAIIIIAAAgjoC/w/mDOVH0wNG5MAAAAASUVORK5CYII=
			--------------040305040006080906090803--
			
			--------------090707030607080209050905--
			|] -- warning I truncated the base64 picture attachments
	let msg = MboxMessage "sender" "Jan 23 06:00:05 2013" source "file" 0
	let expected = Email
		{
			date = LocalTime (fromGregorian 2013 10 3) (TimeOfDay 10 18 40),
			to = "c d <c@test.com>",
			cc = Nothing,
			subject = "Re: FW: Test",
			contents = "<html>contents HTML\n</html>\n<hr/>"
		}
	assertEqual "doesn't match" expected (messageToEmail' msg)

testDifferentMessage :: Spec
testDifferentMessage = it "parses a different message" $ do
	let source = BL.pack $ T.unpack [strT|
			From - Wed Nov 13 21:47:51 2013
			X-Account-Key: account3
			X-UIDL: 5772.NiG4Gf0oqFthpgYGWYC9gzRwsIxdwVaV3p4Jg11nyGk=
			X-Mozilla-Status: 0011
			X-Mozilla-Status2: 00000000
			X-Mozilla-Keys:                                                                                 
			From: "A B" <a.b@c>
			To: "'C D'" <c.d@e>
			Subject: RE: FW: Test
			Date: Wed, 13 Nov 2013 13:57:05 +0100
			MIME-Version: 1.0
			Content-Type: multipart/mixed;
				boundary="----=_NextPart_000_00CE_01CEE078.3CBC8C10"
			X-Mailer: Microsoft Outlook 15.0
			
			This is a multipart message in MIME format.
			
			------=_NextPart_000_00CE_01CEE078.3CBC8C10
			Content-Type: text/plain;
				charset="us-ascii"
			Content-Transfer-Encoding: 7bit
			
			Hi,

			several lines.
			
			------=_NextPart_000_00CE_01CEE078.3CBC8C10
			Content-Type: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;
				name="Test Cases Performance 010 11112013.xlsx"
			Content-Transfer-Encoding: base64
			Content-Disposition: attachment;
				filename="Test Cases Performance 010 11112013.xlsx"
			
			UEsDBBQABgAIAAAAIQCq91ikeQEAABQGAAATAAgCW0NvbnRlbnRfVHlwZXNdLnhtbCCiBAIooAAC
			ZG9jUHJvcHMvYXBwLnhtbFBLBQYAAAAAHwAfACEIAACQnBoAAAA=
			
			------=_NextPart_000_00CE_01CEE078.3CBC8C10--
			|] -- warning I truncated the base64 picture attachments
	let msg = MboxMessage "sender" "Jan 23 06:00:05 2013" source "file" 0
	let expected = Email
		{
			date = LocalTime (fromGregorian 2013 11 13) (TimeOfDay 13 57 5),
			to = "\"'C D'\" <c.d@e>",
			cc = Nothing,
			subject = "RE: FW: Test",
			contents = "Hi,\n<br/>several lines.\n<br/><hr/><p><a href='AttachmentKey {mailId = \"\", attachmentIndex = 1}' class='btn btn-default' role='button'><span class='glyphicon glyphicon-paperclip'></span>Test Cases Performance 010 11112013.xlsx</a></p>"
		}
	-- also cover the alternate order of parameters in the json... ugly but will do for now.
	let expected2 = expected { contents = "Hi,\n<br/>several lines.\n<br/><hr/><p><a href='AttachmentKey {attachmentIndex = 1, mailId = \"\"}' class='btn btn-default' role='button'><span class='glyphicon glyphicon-paperclip'></span>Test Cases Performance 010 11112013.xlsx</a></p>" }
	let parsed = messageToEmail' msg
	assertBool ("doesn't match; parsed: "
		++ show parsed
		++ "\nexpected:\n" ++ show expected
		++ "\nor\n" ++ show expected2) (parsed == expected || parsed == expected2)

getMultipartBodyText :: T.Text -> BL.ByteString -> T.Text
getMultipartBodyText sep bdy = fromMaybe "no body!" 
	(sectionTextContent <$> (parseMultipartBody sep bdy >>= sectionToConsider))

messageToEmail' :: MboxMessage BL.ByteString -> Email
messageToEmail' = messageToEmail show
