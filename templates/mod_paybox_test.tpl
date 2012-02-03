{% extends "base.tpl" %}

{% block title %}{_ Run a Test Transaction _}{% endblock %}

{% block content %}

   <h1>Create a Test Order! </h1>
    {% wire id="my-order-form" type="submit" postback={create_order_example} delegate="mod_paybox" %}
    <form id="my-order-form" method="post" action="postback" delegate="mod_paybox" postback="create_order_example">
        <fieldset class="zp-100">
            <div class="zp-30">
                <div class="form-item">
                    <label for="email">{_ Email Address _}</label>
                    <input type="text" id="email" name="email" />
                    {% validate id="email" type={presence} type={email} %}
                </div>
                <div class="form-item">
                    <label for="product">{_ Product Id (1 costs 1 Euro, 2 costs 2 Euro ) _}</label>
                    <input type="text" id="product" name="product" value="1" />
                    {% validate id="product" type={presence}  %}
                </div>
                <div class="form-item">
                    <button id="mybutton" type="submit">Pay Now</button>
                </div>
        </fieldset>
    </form>   


{% endblock %}

