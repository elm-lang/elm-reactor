Elm.Native.Slider = {};
Elm.Native.Slider.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Slider = localRuntime.Native.Slider || {};
    if (localRuntime.Native.Slider.values) {
        return localRuntime.Native.Slider.values;
    }

    var Element = Elm.Graphics.Element.make(localRuntime);
    var NativeElement = Elm.Native.Graphics.Element.make(localRuntime);

    function renderSlider(model) {
        var node = NativeElement.createNode('input');
        node.type = 'range';

        node.min = model.styling.min;
        node.max = model.styling.max;
        node.step = model.styling.step;
        node.value = model.styling.value;

        if (!model.styling.horizontal) {
            node.orient = "vertical"; // FF
            node.style.webkitAppearance = "slider-vertical"; // webkit
            node.style.writingMode = "bt-lr"; // ie
        }

        if (model.styling.disabled) {
            node.disabled = true;
        }

        node.style.display = 'block';
        node.style.pointerEvents = 'auto';
        node.elm_handler = model.handler;
        node.addEventListener('input', notifySlider);
        node.addEventListener('change', notifySlider);
        function notifySlider() {
            node.elm_handler(node.value)();
        }
        return node;
    }

    function updateSlider(node, oldModel, newModel) {
        if (newModel.styling.disabled) {
            node.disabled = true;
        } else {
            node.disabled = false;
        }
        node.elm_handler = newModel.handler;
        node.min = newModel.styling.min;
        node.max = newModel.styling.max;
        node.step = newModel.styling.step;
        node.value = newModel.styling.value;

        return node;
    }

    function slider(handler, styling) {
        var width = styling.length;
        var height = 24;
        if (!styling.horizontal) {
            var temp = width;
            width = height;
            height = temp;
        }
        return A3(NativeElement.newElement, width, height, {
            ctor: 'Custom',
            type: 'Slider',
            render: renderSlider,
            update: updateSlider,
            model: { handler:handler, styling:styling }
        });
    }

    return localRuntime.Native.Slider.values = {
        slider: F2(slider)
    };
}
