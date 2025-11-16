# sidescroll

A modern minimap mode for Emacs that provides a bird's-eye view of your buffer.

## Overview

Sidescroll creates a minimap-style side window that displays the same buffer with a much smaller font, allowing you to see an overview of the entire file while editing. Unlike the old Emacs minimap mode, sidescroll uses modern Emacs practices and is designed for recent Emacs versions.

## Features

- **Customizable positioning**: Display minimap on the left or right side
- **Customizable face**: Full control over minimap appearance through `sidescroll-face`
- **Bidirectional synchronization**: Scrolling in either window updates the other
- **Drag-scroll navigation**: Click and drag in the minimap to scroll the main buffer
- **Automatic cursor synchronization**: Cursor position stays in sync between main buffer and minimap
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

### `sidescroll-face`

A customizable face that controls the appearance of the minimap text. The `:height` attribute controls the size, where lower values show more content (default: `20` means 0.2x normal size). You can also customize other face attributes like foreground, background, font family, etc.

```elisp
;; Customize just the height
(set-face-attribute 'sidescroll-face nil :height 30)

;; Or customize multiple attributes
(custom-set-faces
 '(sidescroll-face ((t (:height 20 :foreground "gray50")))))
```

### `sidescroll-window-width`

Width of the minimap window in columns (default: `30`).

```elisp
(setq sidescroll-window-width 40)
```

### `sidescroll-font-size` (deprecated)

This variable is deprecated in favor of customizing `sidescroll-face`. It's maintained for backward compatibility but using the face is preferred.

## Requirements

- Emacs 26.1 or later

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.
