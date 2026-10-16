import html
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CARD_DATA = ROOT / "src" / "data" / "cards.asm"
GFX_DATA = ROOT / "src" / "gfx.asm"
TEXT_DIR = ROOT / "src" / "text"
EXCLUDED_TEXT_FILE = TEXT_DIR / "text_offsets.asm"
OUTPUT_FILE = ROOT / "docs" / "index.html"

LABEL_PATTERN = re.compile(r"^([A-Za-z_][A-Za-z0-9_]*):\s*$")
TEXT_PATTERN = re.compile(r'^\s*(?:text|textfw|line|para)\s+"(.*)"\s*$', re.IGNORECASE)
FIELD_PATTERN = re.compile(r"^\s*(?:db|dw|tx|energy|gfx)\s+(.+?)\s*$", re.IGNORECASE)
GFX_LABEL_PATTERN = re.compile(r"^([A-Za-z_][A-Za-z0-9_]*)::?\s*$")
CARD_ART_PATTERN = re.compile(r'^\s*INCBIN\s+"(gfx/cards/[^"]+)\.2bpp"\s*$', re.IGNORECASE)

TYPE_NAMES = {
    "FIRE": "Fire",
    "GRASS": "Grass",
    "LIGHTNING": "Lightning",
    "WATER": "Water",
    "FIGHTING": "Fighting",
    "PSYCHIC": "Psychic",
    "COLORLESS": "Colorless",
    "UNUSED": "Unused",
}
STAGE_NAMES = {
    "BASIC": "Basic",
    "STAGE1": "Stage 1",
    "STAGE2": "Stage 2",
    "STAGE2_WITHOUT_STAGE1": "Stage 2",
}
WEAKNESS_NAMES = {
    "WR_FIRE": "Fire",
    "WR_GRASS": "Grass",
    "WR_LIGHTNING": "Lightning",
    "WR_WATER": "Water",
    "WR_FIGHTING": "Fighting",
    "WR_PSYCHIC": "Psychic",
}


def split_records(lines: list[str]):
    records = []
    label = None
    record_lines = []

    for line in lines:
        match = LABEL_PATTERN.match(line)
        if match:
            if label is not None:
                records.append((label, record_lines))
            label = match.group(1)
            record_lines = []
        elif label is not None:
            record_lines.append(line)

    if label is not None:
        records.append((label, record_lines))
    return records


def parse_card_art_paths(gfx_lines):
    art_paths = {}
    label = None
    for line in gfx_lines:
        label_match = GFX_LABEL_PATTERN.match(line)
        if label_match:
            label = label_match.group(1)
            continue
        art_match = CARD_ART_PATTERN.match(line)
        if label and art_match:
            art_paths[label] = Path(art_match.group(1) + ".png")
    return art_paths


def parse_text_files(text_files):
    text_by_label = {}
    for text_file in text_files:
        for label, lines in split_records(text_file.read_text(encoding="utf-8").splitlines()):
            text_lines = []
            for line in lines:
                if re.match(r"^\s*done\b", line, re.IGNORECASE):
                    break
                match = TEXT_PATTERN.match(line)
                if match:
                    text_lines.append(match.group(1))
            if text_lines:
                text_by_label[label] = " ".join(text_lines)
    return text_by_label


def field_value(lines, field_name):
    annotation = re.compile(r";\s*" + re.escape(field_name) + r"\s*$", re.IGNORECASE)
    for line in lines:
        code, separator, comment = line.partition(";")
        if separator and annotation.search(";" + comment.strip()):
            match = FIELD_PATTERN.match(code)
            if match:
                return match.group(1).strip()
    return "NONE"


def resolve_text(value, text_by_label):
    symbol = value.strip()
    if symbol.upper() == "NONE":
        return ""
    return text_by_label.get(symbol, symbol)


def friendly_type(value: str, prefix="TYPE_PKMN_"):
    name = value.strip().removeprefix(prefix)
    return TYPE_NAMES.get(name, value.strip())


def format_energy(value: str):
    parts = [part.strip() for part in value.split(",")]
    if parts == ["0"] or parts == ["NONE"]:
        return "—"
    if len(parts) % 2:
        return html.escape(value)
    icons = []
    for energy_type, count in zip(parts[::2], parts[1::2]):
        energy_type = energy_type.upper()
        if energy_type not in TYPE_NAMES:
            return html.escape(value)
        try:
            amount = int(count)
        except ValueError:
            return html.escape(value)
        if amount < 0:
            return html.escape(value)
        name = TYPE_NAMES[energy_type]
        for _ in range(amount):
            icons.append(
                f'<img class="tcg-energy-icon" '
                f'src="gfx/{energy_type}.png" alt="{html.escape(name)} energy">'
            )
    return " ".join(icons)


def format_weakness_or_resistance(value: str, suffix=""):
    if value.strip().upper() == "NONE":
        return "—"
    icons = []
    for part in value.split("|"):
        weakness_type = part.strip().upper()
        name = WEAKNESS_NAMES.get(weakness_type)
        if name is None:
            return html.escape(value)
        energy_type = weakness_type.removeprefix("WR_")
        icons.append(
            f'<img class="tcg-energy-icon" '
            f'src="gfx/{energy_type}.png" alt="{html.escape(name)}">'
        )
    if suffix:
        icons.append(html.escape(suffix))
    return " ".join(icons)


def html_value(value):
    return html.escape(value).replace("\n", "<br>\n")


def format_retreat_cost(value: str):
    value = value.strip()
    if value.upper() in {"NONE", "", "0"}:
        return "—"
    try:
        amount = int(value)
    except ValueError:
        return html_value(value)
    if amount <= 0:
        return "—"
    return " ".join(
        '<img class="tcg-energy-icon" src="gfx/COLORLESS.png" alt="Colorless energy">'
        for _ in range(amount)
    )


def render_attack(lines, text_by_label):
    fields = {
        name: field_value(lines, name)
        for name in ("energies", "name", "description", "damage", "category")
    }
    name = resolve_text(fields["name"], text_by_label)
    if not name:
        return ""
    description = resolve_text(fields["description"], text_by_label)
    damage = fields["damage"].strip()
    category = fields["category"].strip()
    # CATEGORY_NAMES:
    #   DAMAGE_NORMAL, DAMAGE_PLUS, DAMAGE_MINUS, DAMAGE_X, POKEMON_POWER
    energies = fields["energies"].strip()
    energy_cost = "Pokémon Power" if category == "POKEMON_POWER" else format_energy(energies)
    damage_value = damage if damage and damage.upper() != "NONE" and damage != "0" else ""
    if damage_value:
        if category == "DAMAGE_PLUS":
            damage_value += "+"
        elif category == "DAMAGE_MINUS":
            damage_value += "-"
        elif category == "DAMAGE_X":
            damage_value += "×"
    # Replace tags in description with actual icons
    description = html.escape(description)
    description = re.sub(
        r"\&lt;([A-Za-z0-9_]+)\&gt;",
        lambda m: f'<img class="tcg-energy-icon" src="gfx/{m.group(1).upper()}.png" alt="{html.escape(TYPE_NAMES.get(m.group(1).upper(), m.group(1)))} energy">',
        description,
    )
    return (
        '<article class="tcg-attack">\n'
        '  <div class="tcg-attack__cost" aria-label="Energy cost">\n'
        f"    {energy_cost}\n"
        "  </div>\n"
        '  <div class="tcg-attack__body">\n'
        '    <div class="tcg-attack__heading">\n'
        f"      <h3>{html_value(name)}</h3>\n"
        f"      <span class=\"tcg-attack__damage\">{html_value(damage_value)}</span>\n"
        "    </div>\n"
        f'    <p class="tcg-attack__description">{description}</p>\n'
        "  </div>\n"
        "</article>"
    )


def parse_pokemon_cards(card_lines: list[str]):
    cards = []
    for label, lines in split_records(card_lines):
        first_data_line = next(
            (
                line.split(";", 1)[0].strip()
                for line in lines
                if line.split(";", 1)[0].strip()
            ),
            "",
        )
        type_match = re.match(r"db\s+(TYPE_PKMN_[A-Z0-9_]+)\b", first_data_line)
        if not type_match:
            continue

        attack_starts = [
            index
            for index, line in enumerate(lines)
            if re.match(r"\s*;\s*attack [12]\s*$", line, re.IGNORECASE)
        ]
        attack_starts.append(len(lines))
        attack_blocks = []
        for start, end in zip(attack_starts, attack_starts[1:]):
            heading = re.search(r"attack ([12])", lines[start], re.IGNORECASE)
            if heading:
                attack_blocks.append((heading.group(1), lines[start + 1 : end]))

        main_fields = {
            name: field_value(lines, name)
            for name in (
                "name",
                "gfx",
                "hp",
                "stage",
                "pre-evo name",
                "retreat cost",
                "weakness",
                "resistance",
            )
        }
        cards.append((label, type_match.group(1), main_fields, attack_blocks))
    return cards


def render_card(label, card_type, fields, attack_blocks, text_by_label, card_art_paths, active_type):
    name = resolve_text(fields["name"], text_by_label) or label
    pokemon_type = friendly_type(card_type)
    type_id = card_type.removeprefix("TYPE_PKMN_").lower()
    type_icon = card_type.removeprefix("TYPE_PKMN_").upper()
    gfx_pointer = fields["gfx"]
    artwork_path = card_art_paths.get(gfx_pointer)
    if artwork_path is None:
        raise ValueError(f"Card {label} references unknown artwork pointer {gfx_pointer!r}")
    artwork_file = ROOT / "src" / artwork_path
    if not artwork_file.is_file():
        raise FileNotFoundError(f"Artwork file for {label} does not exist: {artwork_file}")
    artwork_src = artwork_path.as_posix()
    stage = STAGE_NAMES.get(fields["stage"], fields["stage"])
    stage_label = f"{stage} Pokémon" if stage else "Pokémon"
    pre_evolution = resolve_text(fields["pre-evo name"], text_by_label)
    evolution_line = f"Evolves from {pre_evolution}" if pre_evolution else ""
    attacks = "\n".join(
        render_attack(attack_lines, text_by_label)
        for _, attack_lines in attack_blocks
    )
    weakness = format_weakness_or_resistance(fields["weakness"], suffix="×2")
    resistance = format_weakness_or_resistance(fields["resistance"], suffix="−20")
    retreat_cost = format_retreat_cost(fields["retreat cost"])
    card_class = f"tcg-card tcg-card--{pokemon_type.lower()}" if pokemon_type else "tcg-card"
    hidden = "" if card_type == active_type else " hidden"
    return (
        f'<article class="{card_class}" data-type="{html.escape(type_id)}"{hidden}>\n'
        '  <div class="tcg-card__surface">\n'
        '    <header class="tcg-card__header">\n'
        '      <div class="tcg-card__identity">\n'
        f'       <h2 class="tcg-card__name">{html_value(name)}</h2>\n'
        '        <div class="tcg-card__vitals">\n'
        f'          <span class="tcg-card__hp">{html_value(fields["hp"])} HP</span>\n'
        f'          <img class="tcg-card__type tcg-energy-icon" '
        f'src="gfx/{html.escape(type_icon)}.png" alt="{html_value(pokemon_type)} type">\n'
        "        </div>\n"
        "      </div>\n"
        f'      <p class="tcg-card__stage">{html_value(stage_label)}</p>\n'
        f'      <p class="tcg-card__evolution">{html_value(evolution_line)}</p>\n'
        "    </header>\n"
        '    <figure class="tcg-card__artwork" aria-label="Artwork">\n'
        f'      <img class="tcg-card__artwork-image" src="{html.escape(artwork_src)}" alt="{html_value(name)} artwork">\n'
        "    </figure>\n"
        '    <section class="tcg-card__attacks" aria-label="Attacks">\n'
        f"{attacks}\n"
        "    </section>\n"
        '    <footer class="tcg-card__footer">\n'
        '      <dl class="tcg-card__attributes">\n'
        f'        <div><dt>Weakness</dt><dd>{weakness}</dd></div>\n'
        f'        <div><dt>Resistance</dt><dd>{resistance}</dd></div>\n'
        f'        <div><dt>Retreat Cost</dt><dd>{retreat_cost}</dd></div>\n'
        "      </dl>\n"
        "    </footer>\n"
        "  </div>\n"
        "</article>"
    )


def render_type_tabs(cards, active_type):
    card_types = dict.fromkeys(card_type for _, card_type, _, _ in cards)
    buttons = []
    for card_type in card_types:
        type_name = friendly_type(card_type)
        type_id = card_type.removeprefix("TYPE_PKMN_")
        icon = (
            f'<img class="tcg-energy-icon" '
            f'src="gfx/{type_id}.png" alt="{html.escape(type_name)} Pokémon">'
        )
        type_id = type_id.lower()
        selected = card_type == active_type
        buttons.append(
            f'<button class="type-tab" id="type-tab-{html.escape(type_id)}" '
            f'type="button" role="tab" aria-controls="card-panel" '
            f'aria-selected="{str(selected).lower()}" '
            f'tabindex="{0 if selected else -1}" data-type="{html.escape(type_id)}">'
            f'{icon} {html_value(type_name)}</button>'
        )
    return "\n".join(buttons)


def main():
    text_files = [
        path
        for path in sorted(TEXT_DIR.rglob("*"))
        if path.is_file() and path != EXCLUDED_TEXT_FILE
    ]
    text_by_label = parse_text_files(text_files)
    cards = parse_pokemon_cards(CARD_DATA.read_text(encoding="utf-8").splitlines())
    card_art_paths = parse_card_art_paths(GFX_DATA.read_text(encoding="utf-8").splitlines())
    active_type = cards[0][1] if cards else ""
    active_type_id = active_type.removeprefix("TYPE_PKMN_").lower()
    tabs = render_type_tabs(cards, active_type)
    sections = "\n".join(
        render_card(label, card_type, fields, attacks, text_by_label, card_art_paths, active_type)
        for label, card_type, fields, attacks in cards
    )
    OUTPUT_FILE.write_text(
        "<!doctype html>\n"
        '<html lang="en">\n'
        "<head>\n"
        '  <meta charset="utf-8">\n'
        '  <meta name="viewport" content="width=device-width, initial-scale=1">\n'
        '  <link rel="stylesheet" href="cards.css">\n'
        "  <title>Card Gallery</title>\n"
        "</head>\n"
        "<body>\n"
        '  <main class="page">\n'
        "    <h1>Card Gallery</h1>\n"
        f'    <nav class="type-tabs" role="tablist" aria-label="Filter cards by type">\n{tabs}\n    </nav>\n'
        f'    <div class="card-grid" id="card-panel" role="tabpanel" '
        f'aria-labelledby="type-tab-{html.escape(active_type_id)}" tabindex="0">\n{sections}\n    </div>\n'
        "  </main>\n"
        "  <script>\n"
        "    const tabs = [...document.querySelectorAll('.type-tab')];\n"
        "    const cards = [...document.querySelectorAll('.tcg-card')];\n"
        "    const panel = document.getElementById('card-panel');\n"
        "    function activateTab(tab, moveFocus = false) {\n"
        "      const selectedType = tab.dataset.type;\n"
        "      tabs.forEach((item) => {\n"
        "        const selected = item === tab;\n"
        "        item.setAttribute('aria-selected', String(selected));\n"
        "        item.tabIndex = selected ? 0 : -1;\n"
        "      });\n"
        "      cards.forEach((card) => {\n"
        "        card.hidden = card.dataset.type !== selectedType;\n"
        "      });\n"
        "      panel.setAttribute('aria-labelledby', tab.id);\n"
        "      if (moveFocus) tab.focus();\n"
        "    }\n"
        "    tabs.forEach((tab, index) => {\n"
        "      tab.addEventListener('click', () => activateTab(tab));\n"
        "      tab.addEventListener('keydown', (event) => {\n"
        "        let nextIndex = index;\n"
        "        if (event.key === 'ArrowRight') nextIndex = (index + 1) % tabs.length;\n"
        "        else if (event.key === 'ArrowLeft') nextIndex = (index - 1 + tabs.length) % tabs.length;\n"
        "        else if (event.key === 'Home') nextIndex = 0;\n"
        "        else if (event.key === 'End') nextIndex = tabs.length - 1;\n"
        "        else return;\n"
        "        event.preventDefault();\n"
        "        activateTab(tabs[nextIndex], true);\n"
        "      });\n"
        "    });\n"
        "  </script>\n"
        "</body>\n"
        "</html>\n",
        encoding="utf-8",
    )


if __name__ == "__main__":
    main()
