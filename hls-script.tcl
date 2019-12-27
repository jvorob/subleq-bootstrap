open_project subleq-hls
set_top run
add_files src/sleqrun_main.c
#add_files -tb main.cpp -cflags "-Wno-unknown-pragmas" -csimflags "-Wno-unknown-pragmas"
#add_files -tb main.h -cflags "-Wno-unknown-pragmas" -csimflags "-Wno-unknown-pragmas"
open_solution "solution1"
set_part {xc7a100t} -tool vivado
create_clock -period 2.5 -name default


# JAN: changed ipname to hopefully change name of generated core
config_export -ipname "subleq" -format ip_catalog -rtl verilog -vendor user.org -vivado_optimization_level 2 -vivado_phys_opt place -vivado_report_level 1


# JAN: `-reset control` (I think) changes to what will memory locations be initialized as
# Low sets them to not be explicitly initialized, which should reduce memory usage a great deal
config_rtl -encoding onehot -kernel_profile=0 -module_auto_prefix=0 -mult_keep_attribute=0 -reset control -reset_async=0 -reset_level low -verbose=1
#config_rtl -encoding onehot -kernel_profile=0 -module_auto_prefix=0 -mult_keep_attribute=0 -reset all -reset_async=0 -reset_level low -verbose=1


#source "directives.tcl"
catch {
    #csim_design
    csynth_design
    #cosim_design -trace_level all
    
    # Don't need this, can manually run export_design and it should use the opts from config_export
    #export_design -rtl verilog -format ip_catalog -vendor "user.org" -ipname "subleq"

    export_design
}
