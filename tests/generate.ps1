$TestSuite = "$PSScriptRoot/../yaml-test-suite";

$Failing = @(
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
    "case_6wpf",
    "case_7a4e",
    "case_7zz5",
    "case_87e4",
    "case_8g76",
    "case_8udb",
    "case_93jh",
    "case_9bxh",
    "case_9c9n",
    "case_9jba",
    "case_9mqt_00",
    "case_9sa2",
    "case_9shh",
    "case_9tfx",
    "case_a2m4",
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
    "case_h2rw",
    "case_hre5",
    "case_j3bt",
    "case_j7vc",
    "case_jhb9",
    "case_jq4r",
    "case_kk5p",
    "case_kss4",
    "case_l9u5",
    "case_le5a",
    "case_lp6e",
    "case_lqz7",
    "case_m2n8_00",
    "case_m5dy",
    "case_mzx3",
    "case_nat4",
    "case_nhx8",
    "case_np9h",
    "case_p94k",
    "case_prh3",
    "case_q88a",
    "case_q8ad",
    "case_qlj7",
    "case_rxy3",
    "case_rzp5",
    "case_rzt7",
    "case_s98z",
    "case_sf5v",
    "case_ssw6",
    "case_t4yy",
    "case_tl85",
    "case_udr7",
    "case_ugm3",
    "case_v9d5",
    "case_w9l4",
    "case_xv9v",
    "case_xw4d",
    "case_zl4z"
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