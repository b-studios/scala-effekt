---
layout: docs
title:  "Guides"
section: "guides"
position: 2
---
# Guides

{% for x in site.pages %}
  {% if x.section == 'guides' and x.title != page.title %}
- [{{x.title}}]({{site.baseurl}}{{x.url}})
  {% endif %}
{% endfor %}
