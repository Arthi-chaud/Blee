import 'dart:async';

import 'package:flutter/gestures.dart';
import 'package:flutter/material.dart';

/// Widget that appears/fades on tap
class HideOnInactivity extends StatefulWidget {
  /// The Widget whose opacity will be messed with
  final Widget child;
  const HideOnInactivity({super.key, required this.child});

  @override
  State<HideOnInactivity> createState() => _HideOnInactivityState();
}

class _HideOnInactivityState extends State<HideOnInactivity> {
  bool isVisible = true;
  Timer? _internalTimer;

  @override
  void initState() {
    super.initState();
    _internalTimer = Timer.periodic(const Duration(seconds: 5), (timer) => setIsVisible(false));
  }

  @override
  void dispose() {
    _internalTimer?.cancel();
    super.dispose();
  }

  void setIsVisible(bool isVisible) {
    setState(() {
      this.isVisible = isVisible;
    });
  }

  void triggerVisibility() {
    setState(() {
      isVisible = true;
      _internalTimer?.cancel();
      _internalTimer = Timer.periodic(
          const Duration(seconds: 5), (timer) => setIsVisible(false));
    });
  }

  @override
  Widget build(BuildContext context) {
    return Listener(
        behavior: HitTestBehavior.opaque,
        // On Click/tap
        onPointerDown: (e) {
          // If tap and controls are already visible
          if (e.kind == PointerDeviceKind.touch &&
              isVisible) {
            setIsVisible(false);
          } else {
            triggerVisibility();
          }
        },
        // on mouse
        onPointerHover: (_) => triggerVisibility(),
        child: AnimatedOpacity(
          opacity: isVisible ? 1.0 : 0.0,
          duration: const Duration(milliseconds: 200),
          child: IgnorePointer(
            ignoring: !isVisible,
            child: widget.child,
          ),
        ));
  }
}
