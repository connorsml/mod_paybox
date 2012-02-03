{% with q.order_number as order_number %}
    {% with m.paybox_order.get[order_number] as order %}
        {{order.redirect_page}}
    {% endwith %}
{% endwith %}