# sidescroll

A modern minimap mode for Emacs that provides a bird's-eye view of your buffer.

## Overview

Sidescroll creates a minimap-style side window that displays the same buffer with a much smaller font, allowing you to see an overview of the entire file while editing. Unlike the old Emacs minimap mode, sidescroll uses modern Emacs practices and is designed for recent Emacs versions.

## Features

- **Customizable positioning**: Display minimap on the left or right side
- **Adjustable font size**: Control how much content is visible in the minimap
- **Automatic synchronization**: Cursor position stays in sync between main buffer and minimap
- **Clean interface**: No line numbers, no modeline in the minimap
- **Buffer change tracking**: Content updates immediately as you edit

## Installation

### Manual Installation

1. Clone this repository or download `sidescroll.el`
2. Add the following to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/sidescroll")
(require 'sidescroll)
```

### Using `use-package`

```elisp
(use-package sidescroll
  :load-path "/path/to/sidescroll")
```

## Usage

Enable sidescroll mode in any buffer:

```elisp
M-x sidescroll-mode
```

Or enable it automatically for specific modes:

```elisp
(add-hook 'prog-mode-hook #'sidescroll-mode)
```

## Customization

Customize the following variables to adjust sidescroll behavior:

### `sidescroll-window-side`

Which side to display the minimap window. Can be `'left` or `'right` (default: `'right`).

```elisp
(setq sidescroll-window-side 'left)
```

### `sidescroll-font-size`

Font size in points for the minimap window (default: `2`). A smaller value shows more content.

```elisp
(setq sidescroll-font-size 3)
```

### `sidescroll-window-width`

Width of the minimap window in columns (default: `30`).

```elisp
(setq sidescroll-window-width 40)
```

## Requirements

- Emacs 26.1 or later

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.
