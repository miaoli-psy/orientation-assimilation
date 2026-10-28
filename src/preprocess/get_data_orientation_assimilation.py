import ast
import os
import glob

import pandas as pd

# relative path
os.chdir(os.path.dirname(os.path.abspath(__file__)))

# configuration
RAW_DIR = "../../data/orientation_assimilation"
OUT_DIR = "../../data"
OUT_NAME = "orientation_assimilation_alldata"
TO_EXCEL = False
INCLUDE_AUDIT = True          # append raw stimulus columns for checking
N_PRACTICE = 3

# ---------------------------------------------------------------------
# need to handel, the same participant got 2 files
#   8894674  -> completed only block 1, then restarted
#   319145802 -> restart, blocks 1-9 (+ aborted trials, dropped below)
# both files -> participant "8894674"; the crash file's block becomes block 10
# ---------------------------------------------------------------------
MERGE_INTO = {"319145802": "8894674", "8894674": "8894674"}
RESTART_PID = "319145802"        # blocks 1-9 come from here
RESTART_DROP_BLOCK = 10          # its block 10 is a stray fragment -> drop
CRASH_PID = "8894674"            # its one block becomes block 10
CRASH_NEW_BLOCK = 10

# tilted-bar position labels, by set size and eccentric rank (slot)
POS_BY_SETSIZE = {
    3: {1: "inner", 2: "middle", 3: "outer"},
    4: {1: "inner", 2: "inner-middle", 3: "outer-middle", 4: "outer"},
}

CORE_COLS = [
    "participant", "age", "sex",
    "block", "trial_in_block",
    "hemifield", "set_size", "tilted_slot", "tilted_position",
    "orientation", "tilt_direction", "target_orientation_signed",
    "target_orientation_folded", "tilt_direction_folded",
    "reported_number", "n_bars_adjusted",
    "adjusted_ori_inner_to_outer", "adjusted_ori_inner_to_outer_folded", "adjusted_ori_LtoR",
    "numerosity_rt", "adjustment_rt",
]
AUDIT_COLS = [
    "n_present_lines", "inner_ecc", "trial_gap",
    "adjusted_indices", "adjust_initial_active_idx", "adjust_active_idx_final",
    "orientation_1_shown", "orientation_2_shown",
    "orientation_3_shown", "orientation_4_shown",
    "shown_posx1", "shown_posx2", "shown_posx3", "shown_posx4",
]
INT_COLS = ["block", "trial_in_block", "set_size", "tilted_slot", "reported_number"]


def parse_list(value):
    """adjust_final_oris / adjusted_indices are saved as python-list strings
    like '[0.0, -3.0]'. Return a list of numbers, or [] if unparseable/NaN."""
    try:
        parsed = ast.literal_eval(str(value))
    except (ValueError, SyntaxError):
        return []
    if isinstance(parsed, (list, tuple)):
        return list(parsed)
    return []


if __name__ == '__main__':
    raw_files = sorted(glob.glob(os.path.join(RAW_DIR, "*.csv")))

    # every participant
    session_list = list()
    for f in raw_files:
        raw_pid = os.path.basename(f).split("_")[0]
        df = pd.read_csv(f)
        df = df.rename(columns={"block": "BLOCK"})   # some exports use lowercase
        df["src_file"] = os.path.basename(f)
        df["src_row"] = range(len(df))
        df["raw_pid"] = raw_pid
        df["participant"] = MERGE_INTO.get(raw_pid, raw_pid)
        session_list.append(df)

    totalData = pd.concat(session_list, ignore_index=True)

    # drop aborted trials (stimulus shown but participant quit before responding)
    # and the restart file's stray block-10 fragment (block 10 = the crash file)
    aborted = totalData["response_number"].isna()
    restart_frag = (totalData["raw_pid"] == RESTART_PID) & (totalData["BLOCK"] == RESTART_DROP_BLOCK)
    print("aborted trials dropped :", int(aborted.sum()))
    print("restart block-10 frag  :", int(restart_frag.sum()))
    totalData = totalData[~(aborted | restart_frag)].copy()

    # merge file for 8894674: crash file's one block becomes block 10
    crash_mask = totalData["raw_pid"] == CRASH_PID
    totalData.loc[crash_mask, "BLOCK"] = CRASH_NEW_BLOCK

    totalData["file_order"] = (totalData["raw_pid"] == CRASH_PID).astype(int)
    totalData = totalData.sort_values(
        ["participant", "file_order", "src_row"]
    ).reset_index(drop=True)

    # remove practice
    totalData["is_practice"] = totalData.groupby("src_file").cumcount() < N_PRACTICE

    main_mask = ~totalData["is_practice"]
    totalData["trial_in_block"] = pd.NA
    totalData.loc[main_mask, "trial_in_block"] = (
        totalData.loc[main_mask].groupby(["participant", "BLOCK"]).cumcount() + 1
    )

    # per-trial derived columns
    records = list()
    for _, row in totalData.iterrows():
        signed = pd.to_numeric(row.get("adjust_target_ori"), errors="coerce")
        set_size = pd.to_numeric(row.get("amount"), errors="coerce")
        n_present = pd.to_numeric(row.get("n_present_lines"), errors="coerce")
        slot = pd.to_numeric(row.get("tilted_line_index"), errors="coerce")
        hemi = row.get("hemisphere")

        final_oris = parse_list(row.get("adjust_final_oris"))   # screen left -> right
        adj_idx = parse_list(row.get("adjusted_indices"))

        # reorder probe list from screen L->R into innermost -> outermost
        if hemi == "left":
            ori_in_out = list(reversed(final_oris))
        else:
            ori_in_out = list(final_oris)

        # tilted-bar position label
        position = ""
        if pd.notna(slot) and pd.notna(set_size):
            position = POS_BY_SETSIZE.get(int(set_size), {}).get(int(slot), "")

        # tilt direction
        if pd.isna(signed) or signed == 0:
            direction = ""
        else:
            direction = "cw" if signed > 0 else "ccw"

        # --------------------------------------------------------------
        # fold hemifield out: mirror left-field trials onto the right-field
        # frame, so a right-field +8 and a left-field -8 (same physical
        # tilt, mirrored across fixation) become the same folded value.
        # + folded = top of the bar tips AWAY from fixation
        # - folded = top of the bar tips TOWARD fixation
        # --------------------------------------------------------------
        fold_sign = -1.0 if hemi == "left" else 1.0
        signed_folded = signed * fold_sign if pd.notna(signed) else pd.NA
        if pd.isna(signed_folded) or signed_folded == 0:
            direction_folded = ""
        else:
            direction_folded = "away" if signed_folded > 0 else "toward"
        ori_in_out_folded = [v * fold_sign for v in ori_in_out]

        rec = {
            "participant": row["participant"],
            "age": row.get("age"),
            "sex": row.get("sex"),
            "session": row.get("date"),
            "src_file": row["src_file"],
            "block": row["BLOCK"],
            "trial_in_block": row["trial_in_block"],
            "is_practice": bool(row["is_practice"]),
            "hemifield": hemi,
            "set_size": set_size,
            "tilted_slot": slot,
            "tilted_position": position,
            "orientation": abs(signed) if pd.notna(signed) else pd.NA,
            "tilt_direction": direction,
            "target_orientation_signed": signed,
            "target_orientation_folded": signed_folded,
            "tilt_direction_folded": direction_folded,
            "reported_number": pd.to_numeric(row.get("response_number"), errors="coerce"),
            "n_bars_adjusted": len(adj_idx),
            "adjusted_ori_inner_to_outer": str(ori_in_out),
            "adjusted_ori_inner_to_outer_folded": str(ori_in_out_folded),
            "adjusted_ori_LtoR": str(list(final_oris)),
            "numerosity_rt": pd.to_numeric(row.get("key_resp.rt"), errors="coerce"),
            "adjustment_rt": pd.to_numeric(row.get("rotation_response.rt"), errors="coerce"),
        }
        if INCLUDE_AUDIT:
            for c in AUDIT_COLS:
                rec[c] = row.get(c)
        records.append(rec)

    processed = pd.DataFrame(records)

    # drop practice trials from the output
    processed = processed[~processed["is_practice"]].reset_index(drop=True)

    out_cols = CORE_COLS + (AUDIT_COLS if INCLUDE_AUDIT else [])
    processed = processed[out_cols]

    for c in INT_COLS:
        processed[c] = pd.to_numeric(processed[c], errors="coerce").astype("Int64")

    # write to csv
    out_csv = os.path.join(OUT_DIR, OUT_NAME + ".csv")
    processed.to_csv(out_csv, index=False)

