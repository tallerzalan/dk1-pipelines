"""What differs between price zones. Everything else in the package is shared, so a new zone is a new entry here."""
from dataclasses import dataclass


@dataclass(frozen=True)
class Site:
    name: str
    lat: float
    lon: float
    kind: str  # "wind" or "solar"


@dataclass(frozen=True)
class Zone:
    code: str
    neighbours: tuple[str, ...]
    sites: tuple[Site, ...]


GERMANY = (
    Site("de_north_sea", 54.20, 6.50, "wind"),
    Site("de_baltic", 54.80, 14.00, "wind"),
    Site("de_schleswig", 54.40, 9.20, "wind"),
    Site("de_lower_saxony", 52.90, 8.00, "wind"),
    Site("de_brandenburg", 53.00, 13.00, "wind"),
    Site("de_bavaria", 48.60, 11.50, "solar"),
    Site("de_baden", 48.70, 9.20, "solar"),
    Site("de_saxony", 51.10, 13.00, "solar"),
)

ZONES = {
    "DK1": Zone("DK1", ("DE", "DK2", "NO2", "SE3", "SE4"), (
        Site("horns_rev", 55.53, 7.91, "wind"),
        Site("west_jutland", 56.10, 8.30, "wind"),
        Site("north_jutland", 57.05, 9.30, "wind"),
        Site("south_jutland", 55.20, 9.20, "solar"),
        Site("mid_jutland", 56.20, 9.50, "solar"),
        Site("funen", 55.30, 10.30, "solar"),
    )),
    "DK2": Zone("DK2", ("DE", "DK1", "NO2", "SE3", "SE4"), (
        Site("kriegers_flak", 55.05, 12.90, "wind"),
        Site("rodsand", 54.55, 11.65, "wind"),
        Site("lolland", 54.80, 11.30, "wind"),
        Site("lolland_falster", 54.80, 11.90, "solar"),
        Site("mid_zealand", 55.45, 11.80, "solar"),
        Site("north_zealand", 55.85, 12.30, "solar"),
    )),
}
