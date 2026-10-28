# orientation_assimilation

This document has two parts:

1. **Task parameters** — everything needed to replicate the task, taken from `orientation_assimilation.py` (the script that collected the data in `data/orientation_assimilation/`).
2. **Data dictionary** — every column in `orientation_assimilation_alldata.csv`, the analysis-ready file produced by `src/preprocess/get_data_orientation_assimilation.py`.


---

## 1. Task overview
Each trial has two parts:

1. **Number report** — 3 or 4 thin vertical bars flash briefly in one peripheral visual field (one of them tilted). The participant reports how many bars they saw.
2. **Orientation reproduction** — a row of vertical bars then appears at the centre of the screen (number of bars matched the numeroisty report from participant), one per bar the participant just reported. The participant rotates at least one of them to reproduce the tilt they perceived.

Only one bar is ever physically tilted; the number report can therefore differ from the true count, and the participant may adjust more than one central bar even though only one was tilted.

## 2. Display setup

| Parameter | Value | Source |
|---|---|---|
| Resolution | 1920 × 1080, fullscreen | `orientation_assimilation.py` |
| Software | PsychoPy 2023.1.1 | `psychopyVersion` field |
| Viewing distance | 57 cm | `mondist` |
| Monitor width | 53 cm (≈ 1 cm / deg) | `monwidth` |
| Refresh rate | 60 Hz | - |
| Background | PsychoPy RGB `[0, 0, 0]` = mid-grey (50%, PsychoPy's ‑1…+1 scale) | code |


## 3. Line stimuli (peripheral array)

Each bar is a `visual.Rect`, not a line primitive: solid black fill and border, so contrast against the grey background is essentially maximal.

| Parameter | Value |
|---|---|
| Width | **0.035°** (fixed) |
| Length (height) | **1.0°** (fixed)  |
| Color | black (`[-1,-1,-1]`) |
| Set size | **3 or 4 bars** |



## 4. Spacing and eccentricity — randomized every trial

This is the part most likely to be mis-copied from older documentation, so it's stated in full.

`orientation.xlsx` contains **template** values (`eccentricity = 10°`, `center_to_center_spacing = 0.775°`, `posx1…posx5`). **The running script overwrites all of these on every trial** — the template values are never actually shown to participants:

```python
trial_gap = random.uniform(0.6, 0.9)   # center-to-center spacing, this trial
inner_ecc = random.uniform(8.0, 9.0)   # eccentricity of the innermost bar, this trial
```

- **Innermost-bar eccentricity:** continuous uniform **8.0°–9.0°**, redrawn every trial.
- **Centre-to-centre spacing:** continuous uniform **0.6°–0.9°**, redrawn every trial, constant across the bars *within* that trial.
- Remaining bars are placed outward from the innermost bar at that spacing, so the outermost bar lands at roughly **9.2°–10.8°** (3-bar arrays) or **9.8°–11.7°** (4-bar arrays).


## 5. Visual field (left / right)

- Chosen at random with **p = .5 independently on every trial** (not counterbalanced within block).
- The peripheral bar slots (`p1…p5` in code, `shown_posx1…5` in data) are **always built innermost → outermost**, regardless of which side is used. `p1` is always the bar nearest fixation.
- The only hemifield effect on the stimulus is the **sign** of the x-position (negative = left field).
- **Bar orientation is not mirrored by hemifield** — a given signed tilt (e.g. +4°) is physically identical on left- and right-field trials.

> **worked example.** `orientation_1_shown…N_shown` and `shown_posx1…N` are in **slot order = eccentric rank** (1 = innermost) in *both* fields. Whether that slot order reads left→right *on screen* depends on the field:
>
> | hemifield | `shown_posx1, 2, 3` (real trial) | slot order 1→2→3 on screen |
> |---|---|---|
> | right | `8.85, 9.58, 10.30` | left → right (slot 1 = leftmost = innermost) |
> | left | `-8.10, -8.97, -9.85` | **right → left** (slot 1 = rightmost = innermost, slot 3 = leftmost = outermost) |
>
> So on left-hemifield trials, reading `orientation_1_shown, 2_shown, 3_shown` in column order goes **innermost → outermost**, tracing **right-to-left** on screen — not left-to-right. Don't re-derive this as "left-to-right = outermost-to-innermost" for left trials; it's "right-to-left = innermost-to-outermost", same eccentric order as the right field, just mirrored on screen.
>
> This is exactly why `adjusted_ori_inner_to_outer` (§13) needs hemifield-dependent reordering and the columns above don't: the *stimulus* columns are already stored by eccentric rank; only the *adjustment probe* is stored by screen position.

## 6. Presentation timing

| Interval | Duration |
|---|---|
| Pre-stimulus fixation (ITI) | drawn uniformly from **{1.0, 1.25, 1.5, 1.75, 2.0} s** |
| Peripheral array | **9 frames**; all bars appear/disappear together |
| Number screen, adjustment screen | self-paced (no time limit) |

The fixation point is visible during the ITI, during the peripheral array (first 9 frames), and during the number screen. It is **not** shown during the central adjustment screen.

## 7. Fixation

Small dark disc: `GratingStim`, circular mask, **8 px diameter (≈ 0.2°)**, black.

## 8. Orientation (tilt) manipulation

- Exactly **one** bar per trial is tilted; all others are vertical (0°).
- Tilt magnitude ∈ **{1°, 2°, 4°, 6°, 8°}** 
- Direction: clockwise (`ori > 0`) or counter-clockwise (`ori < 0`), PsychoPy convention. Each magnitude occurs once per direction per block — **direction is a replication, not an experimental factor** (errors are intended to be folded across direction in analysis).
- **Tilted-bar position**, by eccentric rank (`tilted_slot`, 1-based, 1 = innermost), independent of hemifield:

  | set size | slot 1 | slot 2 | slot 3 | slot 4 |
  |---|---|---|---|---|
  | 3 | inner | middle | outer | — |
  | 4 | inner | inner-middle | outer-middle | outer |

## 9. Responses

Two responses per trial, **always in this order** (not counterbalanced across participants):

1. **Numerosity** — numeric keypad, keys **1–9**. On-screen prompt is just `?`.
2. **Orientation reproduction** — a row of vertical bars appears at screen centre, one per reported count, spaced **3.2° centre-to-centre** (fixed, independent of the peripheral spacing that trial). Controls:
   - **left/right** — rotate the currently selected bar by **1°** (CCW / CW)
   - **up/down** — rotate the currently selected bar by **0.1°** (CCW / CW)
   - **Space** — move the selection to the next bar
   - **Enter** — confirm; disabled until at least one bar has been rotated
   - Participants may adjust more than one bar even though only one was physically tilted.

## 10. Experimental structure

- **Practice:** **3** trials, randomly sampled from the full condition set, run once before the main experiment. 
- **Main experiment:** **10 blocks**. Each block draws from the 70-row condition set (`orientation.xlsx`) and randomly keeps one of each `inner-middle`/`outer-middle` pair for the 4-bar trials, giving **60 trials per block** (30 three-bar + 30 four-bar) → **600 main trials** total.
- Self-paced breaks between blocks; the screen shows block progress (`n/10`).
- **No control task was run** for this dataset — `control.xlsx` and `run_control_trial()` exist in the code but are disabled (commented out).

---

## 11. How `orientation_assimilation_alldata.csv` was built

`src/preprocess/get_data_orientation_assimilation.py` reads every file in `data/orientation_assimilation/`, then:

1. **Merges the split participant.** `8894674` quit after one block and restarted as `319145802`. Both files are relabelled to participant **`8894674`**: blocks 1–9 come from `319145802`, and the `8894674` file's block becomes **block 10**. A stray single leftover trial in `319145802`'s own "block 10" is dropped.
2. **Drops aborted trials** — any trial where the stimulus was shown but the participant never completed the numerosity response (`response_number` missing). This removed 1 trial (in participant `8894674`'s block 7), so that participant has **599** main trials instead of 600.
3. **Drops practice trials** — the first 3 rows of every raw session file.

Result: **27 participants**, **16,199 rows**, one row per completed, non-practice trial (26 participants × 600 + `8894674`'s 599).

---

## 12. `orientation_assimilation_alldata.csv` — column dictionary

### Trial identifiers

| Column | Type | Meaning |
|---|---|---|
| `participant` | int | Participant ID (filename prefix in `data/orientation_assimilation`); the merged participant is `8894674` (§12) |
| `block` | int, 1–10 | Block number |
| `trial_in_block` | int | 1-based position of the trial within its block, counted among completed, non-practice trials only (so it can skip a number where a trial was aborted) |

### Stimulus (what was shown)

| Column | Type | Meaning |
|---|---|---|
| `hemifield` | `left` / `right` | Visual field the array appeared in this trial |
| `set_size` | 3 / 4 | Number of peripheral bars shown |
| `tilted_slot` | int, 1…`set_size` | Eccentric rank of the tilted bar (1 = closest to fixation), same in both hemifields |
| `tilted_position` | text | `inner` / `middle` / `outer` (3-bar) or `inner` / `inner-middle` / `outer-middle` / `outer` (4-bar); derived from `tilted_slot` × `set_size` |
| `orientation` | float, deg | Tilt **magnitude** of the target bar: one of {1, 2, 4, 6, 8} |
| `tilt_direction` | `cw` / `ccw` | Sign of the physical tilt (PsychoPy: `ori` > 0 = clockwise) |
| `target_orientation_signed` | float, deg | Signed target tilt (+ = cw, − = ccw); `orientation` and `tilt_direction` combined |
| `target_orientation_folded` | float, deg | `target_orientation_signed` with hemifield folded out — see "Folding across hemifield" below |
| `tilt_direction_folded` | `away` / `toward` | Direction of `target_orientation_folded`: whether the bar's top tips away from or toward fixation, independent of hemifield |
| `n_present_lines` | int | Bars actually presented (should always equal `set_size`) |
| `inner_ecc` | float, deg | This trial's randomly drawn innermost-bar eccentricity (§4) |
| `trial_gap` | float, deg | This trial's randomly drawn centre-to-centre spacing (§4) |
| `orientation_1_shown`…`orientation_4_shown` | float, deg | Physical orientation actually assigned to peripheral slot `p1`…`p4` (0 = vertical/absent); exactly one is non-zero |
| `shown_posx1`…`shown_posx4` | float, deg | Signed x-position of peripheral slot `p1`…`p4` (negative = left field); 0 for a slot not used that trial (e.g. `shown_posx4` on 3-bar trials) |

### Numerosity response

| Column | Type | Meaning |
|---|---|---|
| `reported_number` | int | Participant's numerosity report (numpad 1–9) |
| `numerosity_rt` | float, s | Reaction time, from numerosity-screen onset to keypress |

### Orientation-adjustment response

| Column | Type | Meaning |
|---|---|---|
| `n_bars_adjusted` | int | Number of central probe bars the participant rotated at least once (can be > the number of bars whose final orientation is non-zero, if one was rotated and then rotated back to vertical) |
| `adjusted_ori_inner_to_outer` | list of float (string) | Final orientation of each probe bar, **length = `reported_number`**, reordered so index 0 = the bar nearest fixation — see reordering rule below |
| `adjusted_ori_inner_to_outer_folded` | list of float (string) | Same as `adjusted_ori_inner_to_outer`, with hemifield folded out element-by-element (same rule as `target_orientation_folded`) — see "Folding across hemifield" below |
| `adjusted_ori_LtoR` | list of float (string) | Same values, in the **raw on-screen left → right** probe order (no reordering, no folding); kept for audit / an alternative mapping |
| `adjusted_indices` | list of int (string) | Raw 0-based indices (screen left → right) of the probe bars the participant touched |
| `adjust_initial_active_idx` | int | Which probe bar was highlighted when the adjustment screen appeared (random) |
| `adjust_active_idx_final` | int | Which probe bar was highlighted when Enter was pressed (not necessarily one that was adjusted) |
| `adjustment_rt` | float, s | Reaction time, from adjustment-screen onset to the confirming Enter press |

**Reordering rule for `adjusted_ori_inner_to_outer`:** the probe row is saved screen left → right. On right-hemifield trials that already reads innermost → outermost, so the list is unchanged. On left-hemifield trials, screen left → right reads outermost → innermost, so the list is **reversed**. This assumes the participant placed their response at the probe position matching the *screen side* of what they saw (a "retinotopic" mapping) — the task gives no instruction on this correspondence, so the assumption cannot be verified from the data. `adjusted_ori_LtoR` is kept so a different mapping can be applied later if needed.

### Folding across hemifield

A right-hemifield trial with target `+8°` and a left-hemifield trial with target `−8°` are the **same physical stimulus**, mirrored across fixation (mirroring a display reverses the screen-sign of a rotation but not its relationship to fixation). The `_folded` columns collapse that mirror symmetry so the two can be treated as one condition:


Right-field `+8` and left-field `−8` both fold to `target_orientation_folded = +8`, `tilt_direction_folded = away` (the tilted bar's top leans away from fixation in both cases). The same fold is applied to the response list so target and response stay in the same frame — e.g. a response that folds to `+6` is a same-direction, undershooting response to a folded `+8` target, regardless of which hemifield the trial was actually shown in.

