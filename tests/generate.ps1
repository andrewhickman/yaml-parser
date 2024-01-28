$TestSuite = "$PSScriptRoot/../yaml-test-suite";

$Failing = @(
    "case_2xxw",
    "case_35kp",
    "case_36f6",
    "case_3rln_00",
    "case_3rln_01",
    "case_3rln_02",
    "case_3rln_03",
    "case_3rln_04",
    "case_3rln_05",
    "case_4cqq",
    "case_4gc6",
    "case_4zym",
    "case_55wf",
    "case_565n",
    "case_5gbf",
    "case_5kje",
    "case_5llu",
    "case_5tym",
    "case_5we3",
    "case_6fwr",
    "case_6hb6",
    "case_6m2f",
    "case_6pbe",
    "case_6wlz",
    "case_6wpf",
    "case_6zkb",
    "case_753e",
    "case_7a4e",
    "case_7w2p",
    "case_7z25",
    "case_7zz5",
    "case_87e4",
    "case_8g76",
    "case_8udb",
    "case_93jh",
    "case_9bxh",
    "case_9c9n",
    "case_9dxl",
    "case_9jba",
    "case_9mqt_00",
    "case_9sa2",
    "case_9shh",
    "case_9tfx",
    "case_9wxw",
    "case_a2m4",
    "case_a984",
    "case_ab8u",
    "case_bu8l",
    "case_ctn5",
    "case_cvw2",
    "case_dbg4",
    "case_de56_00",
    "case_de56_01",
    "case_de56_02",
    "case_de56_03",
    "case_de56_04",
    "case_de56_05",
    "case_dk95_02",
    "case_dk95_08",
    "case_f8f9",
    "case_g4rs",
    "case_gh63",
    "case_h2rw",
    "case_hre5",
    "case_hwv9",
    "case_j3bt",
    "case_j7vc",
    "case_jhb9",
    "case_jq4r",
    "case_jtv5",
    "case_kk5p",
    "case_kss4",
    "case_l94m",
    "case_l9u5",
    "case_le5a",
    "case_lp6e",
    "case_lqz7",
    "case_m29m",
    "case_m2n8_00",
    "case_m2n8_01",
    "case_m5dy",
    "case_m7a3",
    "case_myw6",
    "case_mzx3",
    "case_nat4",
    "case_nhx8",
    "case_np9h",
    "case_p94k",
    "case_prh3",
    "case_pw8x",
    "case_q88a",
    "case_q8ad",
    "case_q9wf",
    "case_qlj7",
    "case_qt73",
    "case_rr7f",
    "case_rtp8",
    "case_rxy3",
    "case_rzp5",
    "case_rzt7",
    "case_s4t7",
    "case_s98z",
    "case_s9e8",
    "case_sf5v",
    "case_ske5",
    "case_ssw6",
    "case_t4yy",
    "case_tl85",
    "case_u9ns",
    "case_udr7",
    "case_ugm3",
    "case_ut92",
    "case_v9d5",
    "case_vjp3_01",
    "case_w4tn",
    "case_w9l4",
    "case_x8dw",
    "case_xv9v",
    "case_xw4d",
    "case_y79y_002",
    "case_zl4z",
    "case_zwk4"
)

Push-Location $TestSuite
try
{
    Get-ChildItem "$TestSuite/**/in.yaml" -Recurse -Exclude tags | ForEach-Object {
        $Name = Split-Path -Parent (Resolve-Path -Path $_ -Relative)
        $TestName = "case_" + ($Name -replace '[/\\.]','_').Trim('_').ToLower()

        if ($TestName -in $Failing) {
            $SkipArg = ", skip: true"
        } else {
            $SkipArg = ""
        }
        if (Test-Path (Join-Path (Split-Path -Parent $_.FullName) "error")) {
            $FailArg = ", fail: true"
        } else {
            $FailArg = ""
        }

        "case!($TestName, r#`"$Name`"#$SkipArg$FailArg);"
    } | Set-Content "$PSScriptRoot/suite/cases.rs"
}
finally {
    Pop-Location
}