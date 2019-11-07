open_project subleq-hls
set_top run
add_files src/sleqrun_main.c
#add_files -tb main.cpp -cflags "-Wno-unknown-pragmas" -csimflags "-Wno-unknown-pragmas"
#add_files -tb main.h -cflags "-Wno-unknown-pragmas" -csimflags "-Wno-unknown-pragmas"
open_solution "solution1"
set_part {xc7a100t} -tool vivado
create_clock -period 2.5 -name default
config_export -format ip_catalog -rtl verilog -vendor user.org -vivado_optimization_level 2 -vivado_phys_opt place -vivado_report_level 1
#config_rtl -encoding onehot -kernel_profile=0 -module_auto_prefix=0 -mult_keep_attribute=0 -reset all -reset_async=0 -reset_level low -verbose=1
config_rtl -encoding onehot -kernel_profile=0 -module_auto_prefix=0 -mult_keep_attribute=0 -reset control -reset_async=0 -reset_level low -verbose=1
#source "directives.tcl"
catch {
    #csim_design
    csynth_design
    #cosim_design -trace_level all
    #export_design -rtl verilog -format ip_catalog -vendor "user.org"
}
