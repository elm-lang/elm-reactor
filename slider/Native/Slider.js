Elm.Native.Slider = {};
Elm.Native.Slider.make = function(elm) {

    elm.Native = elm.Native || {};
    elm.Native.Slider = elm.Native.Slider || {};
    if (elm.Native.Slider.values) return elm.Native.Slider.values;

    var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;
    var newElement = Elm.Graphics.Element.make(elm).newElement;

    function renderSlider(model) {
        var node = newNode('input');
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
        node.elm_signal = model.signal;
        node.elm_handler = model.handler;
        node.addEventListener('input', function() {
            elm.notify(node.elm_signal.id, node.elm_handler(node.value));
        });
        return node;
    }

    function updateSlider(node, oldModel, newModel) {
        if (newModel.styling.disabled) {
            node.disabled = true;
        } else {
            node.disabled = false;
        }
        node.elm_signal = newModel.signal;
        node.elm_handler = newModel.handler;
        node.min = newModel.styling.min;
        node.max = newModel.styling.max;
        node.step = newModel.styling.step;
        node.value = newModel.styling.value;
    }

    function slider(signal, handler, styling) {
        var width = styling.length;
        var height = 24;
        if (!styling.horizontal) {
            var temp = width;
            width = height;
            height = temp;
        }
        return A3(newElement, width, height, {
            ctor: 'Custom',
            type: 'Slider',
            render: renderSlider,
            update: updateSlider,
            model: { signal:signal, handler:handler, styling:styling }
        });
    }

    return elm.Native.Slider.values = {
        slider:F3(slider)
    };
}
