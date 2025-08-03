const std = @import("std");
const afl = @import("afl_kit");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const zbench_module = b.dependency("zbench", .{
        .target = target,
        .optimize = optimize,
    }).module("zbench");

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    exe_mod.addImport("zbench", zbench_module);

    const exe = b.addExecutable(.{
        .name = "brewdb",
        .root_module = exe_mod,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_module = exe_mod,
    });

    const lldb = b.addSystemCommand(&.{ "gdb", "-tui", "--" });
    lldb.addArtifactArg(exe_unit_tests);

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const test_debug_step = b.step("test-debug", "Run unit tests under lldb");
    test_debug_step.dependOn(&lldb.step);

    const fuzz = b.step("fuzz", "Generate an instrumented executable for AFL++");
    const afl_obj = b.addObject(.{
        .name = "brewdb_fuzz_obj",
        .root_source_file = b.path("src/fuzz.zig"),
        .target = target,
        .optimize = .Debug,
    });
    afl_obj.root_module.stack_check = false;
    afl_obj.root_module.link_libc = true;
    afl_obj.root_module.fuzz = true;
    const afl_fuzz = afl.addInstrumentedExe(b, target, optimize, null, true, afl_obj);
    if (afl_fuzz) |f| fuzz.dependOn(&b.addInstallBinFile(f, "brewdb-afl").step);
}
