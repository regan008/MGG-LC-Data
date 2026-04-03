/**
 * MGG Gaia's Guide — Google Sheets Setup Script
 *
 * TWO WAYS TO USE:
 *
 * A) Single sheet (manual import):
 *    1. Import a canonical CSV into a new Google Sheet
 *       (File → Import → Upload → Replace current sheet)
 *    2. Open Tools → Apps Script, paste this file, save
 *    3. Run → setupGGSheet
 *
 * B) Batch — all CSVs in a Drive folder:
 *    1. Upload your canonical CSVs to a Google Drive folder
 *    2. Open any Google Sheet → Tools → Apps Script, paste this file, save
 *    3. Set FOLDER_ID at the top of setupFolderSheets() to your folder's ID
 *       (copy it from the folder's URL: drive.google.com/drive/folders/FOLDER_ID)
 *    4. Run → setupFolderSheets
 *    5. New Sheets are created in the same Drive folder, one per CSV
 *
 * The script finds columns by header name, so column order doesn't matter.
 */


// ─────────────────────────────────────────────────────────────────────────────
// SINGLE-SHEET ENTRY POINT
// Run this when you've already imported a CSV into the active sheet.
// ─────────────────────────────────────────────────────────────────────────────

function setupGGSheet() {
  var active = SpreadsheetApp.getActiveSpreadsheet();
  if (!active) {
    throw new Error(
      'setupGGSheet must be run from inside a Google Sheet (Tools → Apps Script), ' +
      'not from a standalone script. To process a whole folder, run setupFolderSheets instead.'
    );
  }
  var sheet = active.getActiveSheet();
  applySheetSetup(sheet);
  SpreadsheetApp.getUi().alert(completionMessage());
}


// ─────────────────────────────────────────────────────────────────────────────
// BATCH ENTRY POINT
// Reads every CSV in a Drive folder, creates a new Sheet for each, applies setup.
// ─────────────────────────────────────────────────────────────────────────────

function setupFolderSheets() {
  // ── Paste your folder ID here ──────────────────────────────────────────────
  // Open the folder in Drive; the ID is the long string at the end of the URL:
  // https://drive.google.com/drive/folders/THIS_PART_HERE
  var FOLDER_ID = '1dXsE6FszsXyCiYHMb0m-z2vK9Ow99x9q';
  // ──────────────────────────────────────────────────────────────────────────

  var folder = DriveApp.getFolderById(FOLDER_ID);
  var files  = folder.getFilesByType(MimeType.CSV);
  var count  = 0;
  var names  = [];

  while (files.hasNext()) {
    var file = files.next();
    var name = file.getName().replace(/\.csv$/i, '');

    // Parse the CSV content
    var csv  = file.getBlob().getDataAsString('UTF-8');
    var data = Utilities.parseCsv(csv);

    if (data.length < 2) {
      Logger.log('SKIP (empty): ' + file.getName());
      continue;
    }

    // Create a new spreadsheet and write the data
    var ss    = SpreadsheetApp.create(name);
    var sheet = ss.getActiveSheet();
    sheet.getRange(1, 1, data.length, data[0].length).setValues(data);

    // Apply all validation, formatting, etc.
    applySheetSetup(sheet);

    // Move the new Sheet into the same folder as the CSVs
    var newFile = DriveApp.getFileById(ss.getId());
    newFile.moveTo(folder);

    count++;
    names.push({ name: name, id: ss.getId() });
    Logger.log('Created: ' + name);
  }

  // Show a toast on the last created sheet (works from standalone scripts,
  // unlike getUi().alert() which requires a container-bound context).
  if (names.length > 0) {
    var last = SpreadsheetApp.openById(names[names.length - 1].id);
    last.toast(
      'Created ' + count + ' sheet(s): ' + names.map(function(n) { return n.name; }).join(', '),
      '✅ Batch setup complete',
      30
    );
  }

  Logger.log('Done. Created ' + count + ' sheet(s): ' + names.map(function(n) { return n.name; }).join(', '));
}


// ─────────────────────────────────────────────────────────────────────────────
// CORE SETUP — applies to any sheet object
// ─────────────────────────────────────────────────────────────────────────────

function applySheetSetup(sheet) {
  var dataRows = Math.max(sheet.getLastRow(), 1000);
  var lastCol  = sheet.getLastColumn();

  // Find every column by its header name
  var headers = sheet.getRange(1, 1, 1, lastCol).getValues()[0];
  var col = {};
  headers.forEach(function(h, i) { col[h] = i + 1; });

  // Helper: data range for a named column (row 2 downward)
  function dataCol(name) {
    if (!col[name]) return null;
    return sheet.getRange(2, col[name], dataRows - 1, 1);
  }

  // ── 1. DROPDOWNS ────────────────────────────────────────────────────────────

  // state — strict
  if (col['state']) {
    dataCol('state').setDataValidation(
      SpreadsheetApp.newDataValidation()
        .requireValueInList([
          'AL','AK','AZ','AR','CA','CO','CT','DC','DE','FL','GA','HI',
          'ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN',
          'MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH',
          'OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA',
          'WV','WI','WY','PR','GU','VI'
        ], true)
        .setAllowInvalid(false)
        .setHelpText('2-letter state/territory code only')
        .build()
    );
  }

  // stars — advisory (too varied to restrict)
  if (col['stars']) {
    dataCol('stars').setDataValidation(
      SpreadsheetApp.newDataValidation()
        .requireValueInList([
          '*', '**', '***', '****', '*****',
          '-',
          'MGM', 'MGM*', 'MGM**',
          '**(**)', '***(*)', '***(**)', '****(*)','****(**)',
          '**(*)', '*(*)', '*(*)','*(**)', '**(*?)',
          '?', '**?', '***?', '****(?)','(****)'
        ], true)
        .setAllowInvalid(true)
        .setHelpText('Star rating as printed. Free entry allowed for unusual notations.')
        .build()
    );
  }

  // transcription.method — strict
  if (col['transcription.method']) {
    dataCol('transcription.method').setDataValidation(
      SpreadsheetApp.newDataValidation()
        .requireValueInList(['manual', 'ai-assisted'], true)
        .setAllowInvalid(false)
        .build()
    );
  }

  // ── 2. CHECKBOXES ───────────────────────────────────────────────────────────
  // insertCheckboxes() clears cells that aren't already boolean TRUE/FALSE.
  // CSV imports these as text strings, so we save values and restore them.

  ['unclear.address', 'statewide.address', 'mentions.race', 'mentions.disability']
    .forEach(function(name) {
      if (!col[name]) return;
      var range  = dataCol(name);
      var values = range.getValues();
      range.insertCheckboxes();
      var restored = values.map(function(row) {
        return [String(row[0]).trim().toUpperCase() === 'TRUE'];
      });
      range.setValues(restored);
    });

  // ── 3. CONDITIONAL FORMATTING ───────────────────────────────────────────────

  var rules = [];

  if (col['city']) {
    rules.push(
      SpreadsheetApp.newConditionalFormatRule()
        .whenCellEmpty()
        .setBackground('#F4CCCC')
        .setRanges([dataCol('city')])
        .build()
    );
  }

  if (col['state']) {
    rules.push(
      SpreadsheetApp.newConditionalFormatRule()
        .whenCellEmpty()
        .setBackground('#F4CCCC')
        .setRanges([dataCol('state')])
        .build()
    );
  }

  if (col['unclear.address']) {
    rules.push(
      SpreadsheetApp.newConditionalFormatRule()
        .whenFormulaSatisfied('=' + columnLetter(col['unclear.address']) + '2=TRUE')
        .setBackground('#FCE5CD')
        .setRanges([dataCol('unclear.address')])
        .build()
    );
  }

  if (col['transcription.method']) {
    rules.push(
      SpreadsheetApp.newConditionalFormatRule()
        .whenTextEqualTo('ai-assisted')
        .setBackground('#CFE2F3')
        .setRanges([dataCol('transcription.method')])
        .build()
    );
  }

  sheet.setConditionalFormatRules(rules);

  // ── 4. FREEZE + HEADER STYLING ──────────────────────────────────────────────

  sheet.setFrozenRows(1);
  sheet.setFrozenColumns(1);

  var headerRange = sheet.getRange(1, 1, 1, lastCol);
  headerRange
    .setFontWeight('bold')
    .setBackground('#D9D9D9')
    .setWrap(false);

  var headerProtect = headerRange.protect();
  headerProtect.setDescription('Header row — do not edit column names');
  headerProtect.setWarningOnly(true);

  if (col['year']) {
    var yearProtect = dataCol('year').protect();
    yearProtect.setDescription('Year should be uniform within this sheet');
    yearProtect.setWarningOnly(true);
  }

  // ── 5. RESIZE ───────────────────────────────────────────────────────────────

  sheet.autoResizeColumns(1, sheet.getLastColumn());
}


// ─────────────────────────────────────────────────────────────────────────────
// HELPERS
// ─────────────────────────────────────────────────────────────────────────────

function completionMessage() {
  return (
    'Applied:\n' +
    '  • Dropdown (strict): state, transcription.method\n' +
    '  • Dropdown (advisory): stars\n' +
    '  • Checkboxes: unclear.address, statewide.address, mentions.race, mentions.disability\n' +
    '  • Conditional formatting: blank city/state (red), unclear.address (orange), ai-assisted (blue)\n' +
    '  • Frozen header row and title column'
  );
}

/**
 * Converts a 1-indexed column number to a letter (1→A, 27→AA, etc.)
 */
function columnLetter(n) {
  var letter = '';
  while (n > 0) {
    var rem = (n - 1) % 26;
    letter  = String.fromCharCode(65 + rem) + letter;
    n       = Math.floor((n - 1) / 26);
  }
  return letter;
}
