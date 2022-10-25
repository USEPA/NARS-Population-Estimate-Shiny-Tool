#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("global.r")

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$html(class = "no-js", lang="en"),
  tags$head(
    HTML(
      "<!-- Google Tag Manager -->
		<script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
		new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
		j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
		'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
		})(window,document,'script','dataLayer','GTM-L8ZB');</script>
		<!-- End Google Tag Manager -->
		"
    ),
    tags$meta(charset="utf-8"),
    tags$meta(property="og:site_name", content="US EPA"),
    #tags$link(rel = "stylesheet", type = "text/css", href = "css/uswds.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/css/uswds.min.css", integrity="sha512-ZKvR1/R8Sgyx96aq5htbFKX84hN+zNXN73sG1dEHQTASpNA8Pc53vTbPsEKTXTZn9J4G7R5Il012VNsDEReqCA==", crossorigin="anonymous", referrerpolicy="no-referrer"),
    tags$meta(property="og:url", content="https://www.epa.gov/themes/epa_theme/pattern-lab/.markup-only.html"),
    tags$link(rel="canonical", href="https://www.epa.gov/themes/epa_theme/pattern-lab/.markup-only.html"),
    tags$link(rel="shortlink", href="https://www.epa.gov/themes/epa_theme/pattern-lab/.markup-only.html"),
    tags$meta(property="og:url", content="https://www.epa.gov/themes/epa_theme/pattern-lab/.markup-only.html"),
    tags$meta(property="og:image", content="https://www.epa.gov/sites/all/themes/epa/img/epa-standard-og.jpg"),
    tags$meta(property="og:image:width", content="1200"),
    tags$meta(property="og:image:height", content="630"),
    tags$meta(property="og:image:alt", content="U.S. Environmental Protection Agency"),
    tags$meta(name="twitter:card", content="summary_large_image"),
    tags$meta(name="twitter:image:alt", content="U.S. Environmental Protection Agency"),
    tags$meta(name="twitter:image:height", content="600"),
    tags$meta(name="twitter:image:width", content="1200"),
    tags$meta(name="twitter:image", content="https://www.epa.gov/sites/all/themes/epa/img/epa-standard-twitter.jpg"),
    tags$meta(name="MobileOptimized", content="width"),
    tags$meta(name="HandheldFriendly", content="true"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$meta(`http-equiv`="x-ua-compatible", content="ie=edge"),
    tags$title('Population Estimate Tool | US EPA'),
    tags$link(rel="icon", type="image/x-icon", href="https://www.epa.gov/themes/epa_theme/images/favicon.ico"),
    tags$meta(name="msapplication-TileColor", content="#FFFFFF"),
    tags$meta(name="msapplication-TileImage", content="https://www.epa.gov/themes/epa_theme/images/favicon-144.png"),
    tags$meta(name="application-name", content=""),
    tags$meta(name="msapplication-config", content="https://www.epa.gov/themes/epa_theme/images/ieconfig.xml"),
    tags$link(rel="apple-touch-icon-precomposed", sizes="196x196", href="https://www.epa.gov/themes/epa_theme/images/favicon-196.png"),
    tags$link(rel="apple-touch-icon-precomposed", sizes="152x152", href="https://www.epa.gov/themes/epa_theme/images/favicon-152.png"),
    tags$link(rel="apple-touch-icon-precomposed", sizes="144x144", href="https://www.epa.gov/themes/epa_theme/images/favicon-144.png"),
    tags$link(rel="apple-touch-icon-precomposed", sizes="120x120", href="https://www.epa.gov/themes/epa_theme/images/favicon-120.png"),
    tags$link(rel="apple-touch-icon-precomposed", sizes="114x114", href="https://www.epa.gov/themes/epa_theme/images/favicon-114.png"),
    tags$link(rel="apple-touch-icon-precomposed", sizes="72x72", href="https://www.epa.gov/themes/epa_theme/images/favicon-72.png"),
    tags$link(rel="apple-touch-icon-precomposed", href="https://www.epa.gov/themes/epa_theme/images/favicon-180.png"),
    tags$link(rel="icon", href="https://www.epa.gov/themes/epa_theme/images/favicon-32.png", sizes="32x32"),
    tags$link(rel="preload", href="https://www.epa.gov/themes/epa_theme/fonts/source-sans-pro/sourcesanspro-regular-webfont.woff2", as="font", crossorigin="anonymous"),
    tags$link(rel="preload", href="https://www.epa.gov/themes/epa_theme/fonts/source-sans-pro/sourcesanspro-bold-webfont.woff2", as="font", crossorigin="anonymous"),
    tags$link(rel="preload", href="https://www.epa.gov/themes/epa_theme/fonts/merriweather/Latin-Merriweather-Bold.woff2", as="font", crossorigin="anonymous"),
    tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/ajax-progress.module.css?r6lsex"),
    tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/autocomplete-loading.module.css?r6lsex" ),
    tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/js.module.css?r6lsex"),
    tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/sticky-header.module.css?r6lsex"),
    tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/system-status-counter.css?r6lsex"),
    tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/system-status-report-counters.css?r6lsex"),
    tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/system-status-report-general-info.css?r6lsex"),
    tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/tabledrag.module.css?r6lsex"),
    tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/tablesort.module.css?r6lsex"),
    tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/core/themes/stable/css/system/components/tree-child.module.css?r6lsex"),
    tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/themes/epa_theme/css/styles.css?r6lsex"),
    tags$link(rel="stylesheet", media="all", href="https://www.epa.gov/themes/epa_theme/css-lib/colorbox.min.css?r6lsex"),
    
    tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/js/uswds-init.min.js'),
    #fix container-fluid that boostrap RShiny uses
    tags$style(HTML(
      '.container-fluid {
            padding-right: 0;
            padding-left: 0;
            margin-right: 0;
            margin-left: 0;
        }
        .tab-content {
            margin-right: 30px;
            margin-left: 30px;
        }'
    ))
  ),
  tags$body(class="path-themes not-front has-wide-template", id="top",
            tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/uswds/3.0.0-beta.3/js/uswds.min.js')
  ),
  
  # Site Header
  HTML(
    '<div class="skiplinks" role="navigation" aria-labelledby="skip-to-main">
      <a id="skip-to-main" href="#main" class="skiplinks__link visually-hidden focusable">Skip to main content</a>
    </div>
    <!-- Google Tag Manager (noscript) -->
    <!--noscript><iframe src="https://www.googletagmanager.com/ns.html?id=GTM-L8ZB" height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript-->
    <!-- End Google Tag Manager (noscript) -->
    <div class="dialog-off-canvas-main-canvas" data-off-canvas-main-canvas>
    <section class="usa-banner" aria-label="Official government website">
      <div class="usa-accordion">
        <header class="usa-banner__header">
          <div class="usa-banner__inner">
            <div class="grid-col-auto">
              <img class="usa-banner__header-flag" src="https://www.epa.gov/themes/epa_theme/images/us_flag_small.png" alt="U.S. flag" />
            </div>
            <div class="grid-col-fill tablet:grid-col-auto">
              <p class="usa-banner__header-text">An official website of the United States government</p>
              <p class="usa-banner__header-action" aria-hidden="true">Here’s how you know</p>
            </div>
            <button class="usa-accordion__button usa-banner__button" aria-expanded="false" aria-controls="gov-banner">
              <span class="usa-banner__button-text">Here’s how you know</span>
            </button>
          </div>
        </header>
        <div class="usa-banner__content usa-accordion__content" id="gov-banner">
          <div class="grid-row grid-gap-lg">
            <div class="usa-banner__guidance tablet:grid-col-6">
              <img class="usa-banner__icon usa-media-block__img" src="https://www.epa.gov/themes/epa_theme/images/icon-dot-gov.svg" alt="Dot gov">
              <div class="usa-media-block__body">
                <p>
                  <strong>Official websites use .gov</strong>
                  <br> A <strong>.gov</strong> website belongs to an official government organization in the United States.
                </p>
              </div>
            </div>
            <div class="usa-banner__guidance tablet:grid-col-6">
              <img class="usa-banner__icon usa-media-block__img" src="https://www.epa.gov/themes/epa_theme/images/icon-https.svg" alt="HTTPS">
              <div class="usa-media-block__body">
                <p>
                  <strong>Secure .gov websites use HTTPS</strong>
                  <br> A <strong>lock</strong> (<span class="icon-lock"><svg xmlns="http://www.w3.org/2000/svg" width="52" height="64" viewBox="0 0 52 64" class="usa-banner__lock-image" role="img" aria-labelledby="banner-lock-title banner-lock-description"><title id="banner-lock-title">Lock</title><desc id="banner-lock-description">A locked padlock</desc><path fill="#000000" fill-rule="evenodd" d="M26 0c10.493 0 19 8.507 19 19v9h3a4 4 0 0 1 4 4v28a4 4 0 0 1-4 4H4a4 4 0 0 1-4-4V32a4 4 0 0 1 4-4h3v-9C7 8.507 15.507 0 26 0zm0 8c-5.979 0-10.843 4.77-10.996 10.712L15 19v9h22v-9c0-6.075-4.925-11-11-11z"/></svg></span>) or <strong>https://</strong> means you’ve safely connected to the .gov website. Share sensitive information only on official, secure websites.
                </p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </section>
    <div>
      <div class="js-view-dom-id-epa-alerts--public">
        <noscript>
          <div class="usa-site-alert usa-site-alert--info">
            <div class="usa-alert">
              <div class="usa-alert__body">
                <div class="usa-alert__text">
                  <p>JavaScript appears to be disabled on this computer. Please <a href="/alerts">click here to see any active alerts</a>.</p>
                </div>
              </div>
            </div>
          </div>
        </noscript>
      </div>
    </div>
    <header class="l-header">
      <div class="usa-overlay"></div>
      <div class="l-constrain">
        <div class="l-header__navbar">
          <div class="l-header__branding">
            <a class="site-logo" href="/" aria-label="Home" title="Home" rel="home">
              <span class="site-logo__image">
                <svg class="site-logo__svg" viewBox="0 0 1061 147" aria-hidden="true" xmlns="http://www.w3.org/2000/svg">
                  <path d="M112.8 53.5C108 72.1 89.9 86.8 69.9 86.8c-20.1 0-38-14.7-42.9-33.4h.2s9.8 10.3-.2 0c3.1 3.1 6.2 4.4 10.7 4.4s7.7-1.3 10.7-4.4c3.1 3.1 6.3 4.5 10.9 4.4 4.5 0 7.6-1.3 10.7-4.4 3.1 3.1 6.2 4.4 10.7 4.4 4.5 0 7.7-1.3 10.7-4.4 3.1 3.1 6.3 4.5 10.9 4.4 4.3 0 7.4-1.2 10.5-4.3zM113.2 43.5c0-24-19.4-43.5-43.3-43.5-24 0-43.5 19.5-43.5 43.5h39.1c-4.8-1.8-8.1-6.3-8.1-11.6 0-7 5.7-12.5 12.5-12.5 7 0 12.7 5.5 12.7 12.5 0 5.2-3.1 9.6-7.6 11.6h38.2zM72.6 139.3c.7-36.9 29.7-68.8 66.9-70 0 37.2-30 68-66.9 70zM67.1 139.3c-.7-36.9-29.7-68.8-67.1-70 0 37.2 30.2 68 67.1 70zM240 3.1h-87.9v133.1H240v-20.4h-60.3v-36H240v-21h-60.3v-35H240V3.1zM272.8 58.8h27.1c9.1 0 15.2-8.6 15.1-17.7-.1-9-6.1-17.3-15.1-17.3h-25.3v112.4h-27.8V3.1h62.3c20.2 0 35 17.8 35.2 38 .2 20.4-14.8 38.7-35.2 38.7h-36.3v-21zM315.9 136.2h29.7l12.9-35h54.2l-8.1-21.9h-38.4l18.9-50.7 39.2 107.6H454L400.9 3.1h-33.7l-51.3 133.1zM473.3.8v22.4c0 1.9.2 3.3.5 4.3s.7 1.7 1 2.2c1.2 1.4 2.5 2.4 3.9 2.9 1.5.5 2.8.7 4.1.7 2.4 0 4.2-.4 5.5-1.3 1.3-.8 2.2-1.8 2.8-2.9.6-1.1.9-2.3 1-3.4.1-1.1.1-2 .1-2.6V.8h4.7v24c0 .7-.1 1.5-.4 2.4-.3 1.8-1.2 3.6-2.5 5.4-1.8 2.1-3.8 3.5-6 4.2-2.2.6-4 .9-5.3.9-1.8 0-3.8-.3-6.2-1.1-2.4-.8-4.5-2.3-6.2-4.7-.5-.8-1-1.8-1.4-3.2-.4-1.3-.6-3.3-.6-5.9V.8h5zM507.5 14.5v-2.9l4.6.1-.1 4.1c.2-.3.4-.7.8-1.2.3-.5.8-.9 1.4-1.4.6-.5 1.4-.9 2.3-1.3.9-.3 2.1-.5 3.4-.4.6 0 1.4.1 2.4.3.9.2 1.9.6 2.9 1.2s1.8 1.5 2.4 2.6c.6 1.2.9 2.8.9 4.7l-.4 17-4.6-.1.4-16c0-.9 0-1.7-.2-2.4-.1-.7-.5-1.3-1.1-1.9-1.2-1.2-2.6-1.8-4.3-1.8-1.7 0-3.1.5-4.4 1.7-1.3 1.2-2 3.1-2.1 5.7l-.3 14.5-4.5-.1.5-22.4zM537.2.9h5.5V6h-5.5V.9m.5 10.9h4.6v25.1h-4.6V11.8zM547.8 11.7h4.3V6.4l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V37c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V15.1h-4.3v-3.4zM570.9 25.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.9-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.6h13zM612.9.9h4.6V33c0 1 .1 2.3.2 4h-4.6l-.1-4c-.2.3-.4.7-.7 1.2-.3.5-.8 1-1.4 1.5-1 .7-2 1.2-3.1 1.4l-1.5.3c-.5.1-.9.1-1.4.1-.4 0-.8 0-1.3-.1s-1.1-.2-1.7-.3c-1.1-.3-2.3-.9-3.4-1.8s-2.1-2.2-2.9-3.8c-.8-1.7-1.2-3.9-1.2-6.6.1-4.8 1.2-8.3 3.4-10.5 2.1-2.1 4.7-3.2 7.6-3.2 1.3 0 2.4.2 3.4.5.9.3 1.6.7 2.2 1.2.6.4 1 .9 1.3 1.4.3.5.6.8.7 1.1V.9m0 23.1c0-1.9-.2-3.3-.5-4.4-.4-1.1-.8-2-1.4-2.6-.5-.7-1.2-1.3-2-1.8-.9-.5-2-.7-3.3-.7-1.7 0-2.9.5-3.8 1.3-.9.8-1.6 1.9-2 3.1-.4 1.2-.7 2.3-.7 3.4-.1 1.1-.2 1.9-.1 2.4 0 1.1.1 2.2.3 3.4.2 1.1.5 2.2 1 3.1.5 1 1.2 1.7 2 2.3.9.6 2 .9 3.3.9 1.8 0 3.2-.5 4.2-1.4 1-.8 1.7-1.8 2.1-3 .4-1.2.7-2.4.8-3.4.1-1.4.1-2.1.1-2.6zM643.9 26.4c0 .6.1 1.3.3 2.1.1.8.5 1.6 1 2.3.5.8 1.4 1.4 2.5 1.9s2.7.8 4.7.8c1.8 0 3.3-.3 4.4-.8 1.1-.5 1.9-1.1 2.5-1.8.6-.7 1-1.5 1.1-2.2.1-.7.2-1.2.2-1.7 0-1-.2-1.9-.5-2.6-.4-.6-.9-1.2-1.6-1.6-1.4-.8-3.4-1.4-5.9-2-4.9-1.1-8.1-2.2-9.5-3.2-1.4-1-2.3-2.2-2.9-3.5-.6-1.2-.8-2.4-.8-3.6.1-3.7 1.5-6.4 4.2-8.1 2.6-1.7 5.7-2.5 9.1-2.5 1.3 0 2.9.2 4.8.5 1.9.4 3.6 1.4 5 3 .5.5.9 1.1 1.2 1.7.3.5.5 1.1.6 1.6.2 1.1.3 2.1.3 2.9h-5c-.2-2.2-1-3.7-2.4-4.5-1.5-.7-3.1-1.1-4.9-1.1-5.1.1-7.7 2-7.8 5.8 0 1.5.5 2.7 1.6 3.5 1 .8 2.6 1.4 4.7 1.9 4 1 6.7 1.8 8.1 2.2.8.2 1.4.5 1.8.7.5.2 1 .5 1.4.9.8.5 1.4 1.1 1.9 1.8s.8 1.4 1.1 2.1c.3 1.4.5 2.5.5 3.4 0 3.3-1.2 6-3.5 8-2.3 2.1-5.8 3.2-10.3 3.3-1.4 0-3.2-.3-5.4-.8-1-.3-2-.7-3-1.2-.9-.5-1.8-1.2-2.5-2.1-.9-1.4-1.5-2.7-1.7-4.1-.3-1.3-.4-2.4-.3-3.2h5zM670 11.7h4.3V6.4l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V37c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V15.1H670v-3.4zM705.3 36.9c-.3-1.2-.5-2.5-.4-3.7-.5 1-1.1 1.8-1.7 2.4-.7.6-1.4 1.1-2 1.4-1.4.5-2.7.8-3.7.8-2.8 0-4.9-.8-6.4-2.2-1.5-1.4-2.2-3.1-2.2-5.2 0-1 .2-2.3.8-3.7.6-1.4 1.7-2.6 3.5-3.7 1.4-.7 2.9-1.2 4.5-1.5 1.6-.1 2.9-.2 3.9-.2s2.1 0 3.3.1c.1-2.9-.2-4.8-.9-5.6-.5-.6-1.1-1.1-1.9-1.3-.8-.2-1.6-.4-2.3-.4-1.1 0-2 .2-2.6.5-.7.3-1.2.7-1.5 1.2-.3.5-.5.9-.6 1.4-.1.5-.2.9-.2 1.2h-4.6c.1-.7.2-1.4.4-2.3.2-.8.6-1.6 1.3-2.5.5-.6 1-1 1.7-1.3.6-.3 1.3-.6 2-.8 1.5-.4 2.8-.6 4.2-.6 1.8 0 3.6.3 5.2.9 1.6.6 2.8 1.6 3.4 2.9.4.7.6 1.4.7 2 .1.6.1 1.2.1 1.8l-.2 12c0 1 .1 3.1.4 6.3h-4.2m-.5-12.1c-.7-.1-1.6-.1-2.6-.1h-2.1c-1 .1-2 .3-3 .6s-1.9.8-2.6 1.5c-.8.7-1.2 1.7-1.2 3 0 .4.1.8.2 1.3s.4 1 .8 1.5.9.8 1.6 1.1c.7.3 1.5.5 2.5.5 2.3 0 4.1-.9 5.2-2.7.5-.8.8-1.7 1-2.7.1-.9.2-2.2.2-4zM714.5 11.7h4.3V6.4l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V37c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V15.1h-4.3v-3.4zM737.6 25.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.9-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.6h13zM765.3 29.5c0 .5.1 1 .2 1.4.1.5.4 1 .8 1.5s.9.8 1.6 1.1c.7.3 1.6.5 2.7.5 1 0 1.8-.1 2.5-.3.7-.2 1.3-.6 1.7-1.2.5-.7.8-1.5.8-2.4 0-1.2-.4-2-1.3-2.5s-2.2-.9-4.1-1.2c-1.3-.3-2.4-.6-3.6-1-1.1-.3-2.1-.8-3-1.3-.9-.5-1.5-1.2-2-2.1-.5-.8-.8-1.9-.8-3.2 0-2.4.9-4.2 2.6-5.6 1.7-1.3 4-2 6.8-2.1 1.6 0 3.3.3 5 .8 1.7.6 2.9 1.6 3.7 3.1.4 1.4.6 2.6.6 3.7h-4.6c0-1.8-.6-3-1.7-3.5-1.1-.4-2.1-.6-3.1-.6h-1c-.5 0-1.1.2-1.7.4-.6.2-1.1.5-1.5 1.1-.5.5-.7 1.2-.7 2.1 0 1.1.5 1.9 1.3 2.3.7.4 1.5.7 2.1.9 3.3.7 5.6 1.3 6.9 1.8 1.3.4 2.2 1 2.8 1.7.7.7 1.1 1.4 1.4 2.2.3.8.4 1.6.4 2.5 0 1.4-.3 2.7-.9 3.8-.6 1.1-1.4 2-2.4 2.6-1.1.6-2.2 1-3.4 1.3-1.2.3-2.5.4-3.8.4-2.5 0-4.7-.6-6.6-1.8-1.8-1.2-2.8-3.3-2.9-6.3h5.2zM467.7 50.8h21.9V55h-17.1v11.3h16.3v4.2h-16.3v12.1H490v4.3h-22.3zM499 64.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V87h-4.6V71c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V87H499V64.7zM524.6 61.8h5.1l7.7 19.9 7.6-19.9h5l-10.6 25.1h-4.6zM555.7 50.9h5.5V56h-5.5v-5.1m.5 10.9h4.6v25.1h-4.6V61.8zM570.3 67c0-1.8-.1-3.5-.3-5.1h4.6l.1 4.9c.5-1.8 1.4-3 2.5-3.7 1.1-.7 2.2-1.2 3.3-1.3 1.4-.2 2.4-.2 3.1-.1v4.6c-.2-.1-.5-.2-.9-.2h-1.3c-1.3 0-2.4.2-3.3.5-.9.4-1.5.9-2 1.6-.9 1.4-1.4 3.2-1.3 5.4v13.3h-4.6V67zM587.6 74.7c0-1.6.2-3.2.6-4.8.4-1.6 1.1-3 2-4.4 1-1.3 2.2-2.4 3.8-3.2 1.6-.8 3.6-1.2 5.9-1.2 2.4 0 4.5.4 6.1 1.3 1.5.9 2.7 2 3.6 3.3.9 1.3 1.5 2.8 1.8 4.3.2.8.3 1.5.4 2.2v2.2c0 3.7-1 6.9-3 9.5-2 2.6-5.1 4-9.3 4-4-.1-7-1.4-9-3.9-1.9-2.5-2.9-5.6-2.9-9.3m4.8-.3c0 2.7.6 5 1.8 6.9 1.2 2 3 3 5.6 3.1.9 0 1.8-.2 2.7-.5.8-.3 1.6-.9 2.3-1.7.7-.8 1.3-1.9 1.8-3.2.4-1.3.6-2.9.6-4.7-.1-6.4-2.5-9.6-7.1-9.6-.7 0-1.5.1-2.4.3-.8.3-1.7.8-2.5 1.6-.8.7-1.4 1.7-1.9 3-.6 1.1-.9 2.8-.9 4.8zM620.2 64.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V87h-4.6V71c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V87h-4.6V64.7zM650 65.1l-.1-3.3h4.6v3.6c1.2-1.9 2.6-3.2 4.1-3.7 1.5-.4 2.7-.6 3.8-.6 1.4 0 2.6.2 3.6.5.9.3 1.7.7 2.3 1.1 1.1 1 1.9 2 2.3 3.1.2-.4.5-.8 1-1.3.4-.5.9-1 1.5-1.6.6-.5 1.5-.9 2.5-1.3 1-.3 2.2-.5 3.5-.5.9 0 1.9.1 3 .3 1 .2 2 .7 3 1.3 1 .6 1.7 1.5 2.3 2.7.6 1.2.9 2.7.9 4.6v16.9h-4.6V70.7c0-1.1-.1-2-.2-2.5-.1-.6-.3-1-.6-1.3-.4-.6-1-1.2-1.8-1.6-.8-.4-1.8-.6-3.1-.6-1.5 0-2.7.4-3.6 1-.4.3-.8.5-1.1.9l-.8.8c-.5.8-.8 1.8-1 2.8-.1 1.1-.2 2-.1 2.6v14.1h-4.6V70.2c0-1.6-.5-2.9-1.4-4-.9-1-2.3-1.5-4.2-1.5-1.6 0-2.9.4-3.8 1.1-.9.7-1.5 1.2-1.8 1.7-.5.7-.8 1.5-.9 2.5-.1.9-.2 1.8-.2 2.6v14.3H650V65.1zM700.5 75.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.8-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.6h13zM725.7 64.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V87h-4.6V71c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V87h-4.6V64.7zM752.3 61.7h4.3v-5.2l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V87c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V65.1h-4.3v-3.4zM787.6 86.9c-.3-1.2-.5-2.5-.4-3.7-.5 1-1.1 1.8-1.7 2.4-.7.6-1.4 1.1-2 1.4-1.4.5-2.7.8-3.7.8-2.8 0-4.9-.8-6.4-2.2-1.5-1.4-2.2-3.1-2.2-5.2 0-1 .2-2.3.8-3.7.6-1.4 1.7-2.6 3.5-3.7 1.4-.7 2.9-1.2 4.5-1.5 1.6-.1 2.9-.2 3.9-.2s2.1 0 3.3.1c.1-2.9-.2-4.8-.9-5.6-.5-.6-1.1-1.1-1.9-1.3-.8-.2-1.6-.4-2.3-.4-1.1 0-2 .2-2.6.5-.7.3-1.2.7-1.5 1.2-.3.5-.5.9-.6 1.4-.1.5-.2.9-.2 1.2h-4.6c.1-.7.2-1.4.4-2.3.2-.8.6-1.6 1.3-2.5.5-.6 1-1 1.7-1.3.6-.3 1.3-.6 2-.8 1.5-.4 2.8-.6 4.2-.6 1.8 0 3.6.3 5.2.9 1.6.6 2.8 1.6 3.4 2.9.4.7.6 1.4.7 2 .1.6.1 1.2.1 1.8l-.2 12c0 1 .1 3.1.4 6.3h-4.2m-.5-12.1c-.7-.1-1.6-.1-2.6-.1h-2.1c-1 .1-2 .3-3 .6s-1.9.8-2.6 1.5c-.8.7-1.2 1.7-1.2 3 0 .4.1.8.2 1.3s.4 1 .8 1.5.9.8 1.6 1.1c.7.3 1.5.5 2.5.5 2.3 0 4.1-.9 5.2-2.7.5-.8.8-1.7 1-2.7.1-.9.2-2.2.2-4zM800.7 50.9h4.6V87h-4.6zM828.4 50.8h11.7c2.1 0 3.9.1 5.5.4.8.2 1.5.4 2.2.9.7.4 1.3.9 1.8 1.6 1.7 1.9 2.6 4.2 2.6 7 0 2.7-.9 5.1-2.8 7.1-.8.9-2 1.7-3.6 2.2-1.6.6-3.9.9-6.9.9h-5.7V87h-4.8V50.8m4.8 15.9h5.8c.8 0 1.7-.1 2.6-.2.9-.1 1.8-.3 2.6-.7.8-.4 1.5-1 2-1.9.5-.8.8-2 .8-3.4s-.2-2.5-.7-3.3c-.5-.8-1.1-1.3-1.9-1.7-1.6-.5-3.1-.8-4.5-.7h-6.8v11.9zM858.1 67c0-1.8-.1-3.5-.3-5.1h4.6l.1 4.9c.5-1.8 1.4-3 2.5-3.7 1.1-.7 2.2-1.2 3.3-1.3 1.4-.2 2.4-.2 3.1-.1v4.6c-.2-.1-.5-.2-.9-.2h-1.3c-1.3 0-2.4.2-3.3.5-.9.4-1.5.9-2 1.6-.9 1.4-1.4 3.2-1.3 5.4v13.3H858V67zM875.5 74.7c0-1.6.2-3.2.6-4.8.4-1.6 1.1-3 2-4.4 1-1.3 2.2-2.4 3.8-3.2 1.6-.8 3.6-1.2 5.9-1.2 2.4 0 4.5.4 6.1 1.3 1.5.9 2.7 2 3.6 3.3.9 1.3 1.5 2.8 1.8 4.3.2.8.3 1.5.4 2.2v2.2c0 3.7-1 6.9-3 9.5-2 2.6-5.1 4-9.3 4-4-.1-7-1.4-9-3.9-1.9-2.5-2.9-5.6-2.9-9.3m4.8-.3c0 2.7.6 5 1.8 6.9 1.2 2 3 3 5.6 3.1.9 0 1.8-.2 2.7-.5.8-.3 1.6-.9 2.3-1.7.7-.8 1.3-1.9 1.8-3.2.4-1.3.6-2.9.6-4.7-.1-6.4-2.5-9.6-7.1-9.6-.7 0-1.5.1-2.4.3-.8.3-1.7.8-2.5 1.6-.8.7-1.4 1.7-1.9 3-.7 1.1-.9 2.8-.9 4.8zM904.1 61.7h4.3v-5.2l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V87c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V65.1h-4.3v-3.4zM927.2 75.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.9-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.6h13zM966.1 69.8c0-.3 0-.8-.1-1.4-.1-.6-.3-1.1-.6-1.8-.2-.6-.7-1.2-1.4-1.6-.7-.4-1.6-.6-2.7-.6-1.5 0-2.7.4-3.5 1.2-.9.8-1.5 1.7-1.9 2.8-.4 1.1-.6 2.2-.7 3.2-.1 1.1-.2 1.8-.1 2.4 0 1.3.1 2.5.3 3.7.2 1.2.5 2.3.9 3.3.8 2 2.4 3 4.8 3.1 1.9 0 3.3-.7 4.1-1.9.8-1.1 1.2-2.3 1.2-3.6h4.6c-.2 2.5-1.1 4.6-2.7 6.3-1.7 1.8-4.1 2.7-7.1 2.7-.9 0-2.1-.2-3.6-.6-.7-.2-1.4-.6-2.2-1-.8-.4-1.5-1-2.2-1.7-.7-.9-1.4-2.1-2-3.6-.6-1.5-.9-3.5-.9-6.1 0-2.6.4-4.8 1.1-6.6.7-1.7 1.6-3.1 2.7-4.2 1.1-1 2.3-1.8 3.6-2.2 1.3-.4 2.5-.6 3.7-.6h1.6c.6.1 1.3.2 1.9.4.7.2 1.4.5 2.1 1 .7.4 1.3 1 1.8 1.7.9 1.1 1.4 2.1 1.7 3.1.2 1 .3 1.8.3 2.6h-4.7zM973.6 61.7h4.3v-5.2l4.5-1.5v6.8h5.4v3.4h-5.4v15.1c0 .3 0 .6.1 1 0 .4.1.7.4 1.1.2.4.5.6 1 .8.4.3 1 .4 1.8.4 1 0 1.7-.1 2.2-.2V87c-.9.2-2.1.3-3.8.3-2.1 0-3.6-.4-4.6-1.2-1-.8-1.5-2.2-1.5-4.2V65.1h-4.3v-3.4zM993.5 50.9h5.5V56h-5.5v-5.1m.5 10.9h4.6v25.1H994V61.8zM1006.1 74.7c0-1.6.2-3.2.6-4.8.4-1.6 1.1-3 2-4.4 1-1.3 2.2-2.4 3.8-3.2 1.6-.8 3.6-1.2 5.9-1.2 2.4 0 4.5.4 6.1 1.3 1.5.9 2.7 2 3.6 3.3.9 1.3 1.5 2.8 1.8 4.3.2.8.3 1.5.4 2.2v2.2c0 3.7-1 6.9-3 9.5-2 2.6-5.1 4-9.3 4-4-.1-7-1.4-9-3.9-1.9-2.5-2.9-5.6-2.9-9.3m4.7-.3c0 2.7.6 5 1.8 6.9 1.2 2 3 3 5.6 3.1.9 0 1.8-.2 2.7-.5.8-.3 1.6-.9 2.3-1.7.7-.8 1.3-1.9 1.8-3.2.4-1.3.6-2.9.6-4.7-.1-6.4-2.5-9.6-7.1-9.6-.7 0-1.5.1-2.4.3-.8.3-1.7.8-2.5 1.6-.8.7-1.4 1.7-1.9 3-.6 1.1-.9 2.8-.9 4.8zM1038.6 64.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V87h-4.6V71c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V87h-4.6V64.7zM479.1 100.8h5.2l14.1 36.1h-5.3l-3.8-9.4h-16.2l-3.8 9.4h-5l14.8-36.1m-4.4 22.7H488l-6.5-17.8-6.8 17.8zM508.7 138.8c.1.7.2 1.4.4 1.9.2.6.5 1.1.9 1.6.8.9 2.3 1.4 4.4 1.5 1.6 0 2.8-.3 3.7-.9.9-.6 1.5-1.4 1.9-2.4.4-1.1.6-2.3.7-3.7.1-1.4.1-2.9.1-4.6-.5.9-1.1 1.7-1.8 2.3-.7.6-1.5 1-2.3 1.3-1.7.4-3 .6-3.9.6-1.2 0-2.4-.2-3.8-.6-1.4-.4-2.6-1.2-3.7-2.5-1-1.3-1.7-2.8-2.1-4.4-.4-1.6-.6-3.2-.6-4.8 0-4.3 1.1-7.4 3.2-9.5 2-2.1 4.6-3.1 7.6-3.1 1.3 0 2.3.1 3.2.4.9.3 1.6.6 2.1 1 .6.4 1.1.8 1.5 1.2l.9 1.2v-3.4h4.4l-.1 4.5v15.7c0 2.9-.1 5.2-.2 6.7-.2 1.6-.5 2.8-1 3.7-1.1 1.9-2.6 3.2-4.6 3.7-1.9.6-3.8.8-5.6.8-2.4 0-4.3-.3-5.6-.8-1.4-.5-2.4-1.2-3-2-.6-.8-1-1.7-1.2-2.7-.2-.9-.3-1.8-.4-2.7h4.9m5.3-5.8c1.4 0 2.5-.2 3.3-.7.8-.5 1.5-1.1 2-1.8.5-.6.9-1.4 1.2-2.5.3-1 .4-2.6.4-4.8 0-1.6-.2-2.9-.4-3.9-.3-1-.8-1.8-1.4-2.4-1.3-1.4-3-2.2-5.2-2.2-1.4 0-2.5.3-3.4 1-.9.7-1.6 1.5-2 2.4-.4 1-.7 2-.9 3-.2 1-.2 2-.2 2.8 0 1 .1 1.9.3 2.9.2 1.1.5 2.1 1 3 .5.9 1.2 1.6 2 2.2.8.7 1.9 1 3.3 1zM537.6 125.2c-.1 2.6.5 4.8 1.7 6.5 1.1 1.7 2.9 2.6 5.3 2.6 1.5 0 2.8-.4 3.9-1.3 1-.8 1.6-2.2 1.8-4h4.6c0 .6-.2 1.4-.4 2.3-.3 1-.8 2-1.7 3-.2.3-.6.6-1 1-.5.4-1 .7-1.7 1.1-.7.4-1.5.6-2.4.8-.9.3-2 .4-3.3.4-7.6-.2-11.3-4.5-11.3-12.9 0-2.5.3-4.8 1-6.8s2-3.7 3.8-5.1c1.2-.8 2.4-1.3 3.7-1.6 1.3-.2 2.2-.3 3-.3 2.7 0 4.8.6 6.3 1.6s2.5 2.3 3.1 3.9c.6 1.5 1 3.1 1.1 4.6.1 1.6.1 2.9 0 4h-17.5m12.9-3v-1.1c0-.4 0-.8-.1-1.2-.1-.9-.4-1.7-.8-2.5s-1-1.5-1.8-2.1c-.9-.5-2-.8-3.4-.8-.8 0-1.5.1-2.3.3-.8.2-1.5.7-2.2 1.3-.7.6-1.2 1.3-1.6 2.3-.4 1-.7 2.2-.8 3.7h13zM562.9 114.7l-.1-2.9h4.6v4.1c.2-.3.4-.8.7-1.2.3-.5.8-1 1.3-1.5.6-.5 1.4-1 2.3-1.3.9-.3 2-.5 3.4-.5.6 0 1.4.1 2.4.2.9.2 1.9.5 2.9 1.1 1 .6 1.8 1.4 2.5 2.5.6 1.2 1 2.7 1 4.7V137h-4.6v-16c0-.9-.1-1.7-.2-2.4-.2-.7-.5-1.3-1.1-1.9-1.2-1.1-2.6-1.7-4.3-1.7-1.7 0-3.1.6-4.3 1.8-1.3 1.2-2 3.1-2 5.7V137h-4.6v-22.3zM607 119.8c0-.3 0-.8-.1-1.4-.1-.6-.3-1.1-.6-1.8-.2-.6-.7-1.2-1.4-1.6-.7-.4-1.6-.6-2.7-.6-1.5 0-2.7.4-3.5 1.2-.9.8-1.5 1.7-1.9 2.8-.4 1.1-.6 2.2-.7 3.2-.1 1.1-.2 1.8-.1 2.4 0 1.3.1 2.5.3 3.7.2 1.2.5 2.3.9 3.3.8 2 2.4 3 4.8 3.1 1.9 0 3.3-.7 4.1-1.9.8-1.1 1.2-2.3 1.2-3.6h4.6c-.2 2.5-1.1 4.6-2.7 6.3-1.7 1.8-4.1 2.7-7.1 2.7-.9 0-2.1-.2-3.6-.6-.7-.2-1.4-.6-2.2-1-.8-.4-1.5-1-2.2-1.7-.7-.9-1.4-2.1-2-3.6-.6-1.5-.9-3.5-.9-6.1 0-2.6.4-4.8 1.1-6.6.7-1.7 1.6-3.1 2.7-4.2 1.1-1 2.3-1.8 3.6-2.2 1.3-.4 2.5-.6 3.7-.6h1.6c.6.1 1.3.2 1.9.4.7.2 1.4.5 2.1 1 .7.4 1.3 1 1.8 1.7.9 1.1 1.4 2.1 1.7 3.1.2 1 .3 1.8.3 2.6H607zM629.1 137.1l-3.4 9.3H621l3.8-9.6-10.3-25h5.2l7.6 19.8 7.7-19.8h5z"/>
                </svg>
              </span>
            </a>
            <button class="usa-menu-btn usa-button l-header__menu-button">Menu</button>
          </div>
          <div class="l-header__search">
            <form class="usa-search usa-search--small usa-search--epa" method="get" action="https://search.epa.gov/epasearch">
              <div role="search">
                <label class="usa-sr-only" for="search-box">Search</label>
                <input class="usa-input" id="search-box" type="search" name="querytext" placeholder="Search EPA.gov">
                <!-- button class="usa-button" type="submit" --> <!-- type="submit" - removed for now to allow other unrendered buttons to render when triggered in RShiny app -->
                <!-- see: https://github.com/rstudio/shiny/issues/2922 -->
                <button class="usa-button usa-search__submit" style="height:2rem;margin:0;padding:0;padding-left:1rem;padding-right:1rem;border-top-left-radius: 0;border-bottom-left-radius: 0;">
                  <span class="usa-sr-only">Search</span>
                </button>
                <input type="hidden" name="areaname" value="">
                <input type="hidden" name="areacontacts" value="">
                <input type="hidden" name="areasearchurl" value="">
                <input type="hidden" name="typeofsearch" value="epa">
                <input type="hidden" name="result_template" value="">
              </div>
            </form>
          </div>
        </div>
      </div>
      <div class="l-header__nav">
        <nav class="usa-nav usa-nav--epa" role="navigation" aria-label="EPA header navigation">
          <div class="usa-nav__inner">
            <button class="usa-nav__close" aria-label="Close">
              <svg class="icon icon--nav-close" aria-hidden="true" role="img">
                <title>Primary navigation</title>
                <use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#close"></use>
              </svg> </button>
            <div class="usa-nav__menu">
               <ul class="menu menu--main">
                <li class="menu__item"><a href="https://www.epa.gov/environmental-topics" class="menu__link">Environmental Topics</a></li>
                <li class="menu__item"><a href="https://www.epa.gov/laws-regulations" class="menu__link" >Laws &amp; Regulations</a></li>
                <li class="menu__item"><a href="https://www.epa.gov/report-violation" class="menu__link" >Report a Violation</a></li>
                <li class="menu__item"><a href="https://www.epa.gov/aboutepa" class="menu__link" >About EPA</a></li>
              </ul>
            </div>
          </div>
        </nav>
      </div>
    </header>
    <main id="main" class="main" role="main" tabindex="-1">'
  ),
  
  
# Template End ------


   shinyjs::useShinyjs(),
   # Application title

# Instructions ------------------------------------------------------------
   # titlePanel(span("NARS Population Estimate Calculation Tool (v. 2.0.1)",
   #            style = "font-weight: bold; font-size: 28px")),
   navbarPage(title=span("NARS Population Estimate Calculation Tool (v. 2.0.1)",
                         style = "font-weight: bold; font-size: 24px"),         
              header = # Individual Page Header
                HTML(
                  '<div class="l-page  has-footer">
      <div class="l-constrain">
        <div class="l-page__header">
          <div class="l-page__header-first">
            <div class="web-area-title"></div>
          </div>
          <div class="l-page__header-last">
            <a href="https://www.epa.gov/national-aquatic-resource-surveys/forms/contact-us-about-national-aquatic-resource-surveys" class="header-link">Contact Us</a>
          </div>
        </div>
        <article class="article">'
                ),

              footer = HTML(
                '</article>
    </div>
    <div class="l-page__footer">
      <div class="l-constrain">
        <p><a href="https://www.epa.gov/national-aquatic-resource-surveys/forms/contact-us-about-national-aquatic-resource-surveys">Contact Us</a> to ask a question, provide feedback, or report a problem.</p>
      </div>
    </div>
  </div>'
              ),
              selected='instructions',position='static-top',
      # Panel with instructions for using this tool
      tabPanel(title='Instructions for Use',value='instructions',
               h2("Overview"),
               p('This Shiny app allows for calculation of population estimates as performed for
               the National Aquatic Resource Surveys (NARS) and the plotting of results.
               Estimates based on categorical and continuous variables are possible.
                 This app does not include all possible options but does allow typical settings
                 used by NARS for creating population estimates.'),
               h3("Instructions for Use"),
               bsCollapsePanel(title = h4(strong("Prepare Data for Analysis")),
               tags$ol(
                 tags$li("Select data file and upload. If the data are to be loaded from a URL, check the
                         box to do so and paste or enter the URL for the file."),
                 tags$li("The variables in that file will populate dropdown lists on that tab."),
                 tags$li("Select variables to serve as site IDs, weights, response variables, and subpopulations
                         (if desired). If only overall or 'national' estimates are desired, check the box for overall analysis."),
                 tags$li("If data are to be used for change analysis, select the variable that distinguishes between design cycles (we usually assume this variable represents year)."),
                tags$li(p("Select the type of variance you want to use. ",
                          strong("Local neighborhood variance"),
                          " uses a site's nearest neighbors to estimate variance, tending to
                          result in smaller variance values than variance based on a simple random sample. This approach is ",
                          em("recommended"),"and is the approach used in
                          NARS estimates. It requires site coordinates to be provided."),
                        tags$ul(
                        tags$li("For local neighborhood variance, select coordinate variables
                                (Albers projection, or some other projection)."),
                        tags$li("For simple random sample (SRS) variance, selecting a stratum variable
                                to better estimate variance is advised but not required. Coordinates are
                                not used with this type of variance."))),
                br(),
                tags$li("You may subset the data for analysis by up to one categorical variable. To do
                        this, select the check box to subset, then select the variable to subset by.
                        Finally, select one or more categories by which to subset data."),
                tags$li("Click on the left hand button to view the full dataset if necessary."),
               tags$li("Click on the right hand button above the data to subset the data before
                       proceeding to the Run Population Estimates tab.")
               )),
               bsCollapsePanel(title = h4(strong("Minimum Requirements for Analysis")),
                               tags$ul(
                                 tags$li("The R package", strong("spsurvey, v.5.0 or later"),
                                         "is required.
                                Be sure to update this package if an older version is
                                already installed." ),
                                 tags$li("All variables must be contained in one file and include site IDs,
                         weights, response variables, subpopulations (if any), and optionally,
                         coordinates and/or design stratum (depending on type of variance desired)." ),
                                 tags$li("All sites included in the dataset should have weight > 0. Any
                                 records with a missing weight or a weight of 0 will be dropped
                                 before analysis."),
                                 tags$li("Input data should include",
                                         strong("only one row per site and year/survey cycle"),
                                         "(based on the variables for site ID and year/survey cycle selected). No within year revisits should be included, and all variables used in analysis should be separate columns in the dataset (i.e., wide format)."),
                                 tags$li("Only delimited files, such as comma- and tab-delimited, are accepted for upload."),
                                 tags$li("If local neighborhood variance is desired, coordinates must be
                                 provided in some type of projection, such as Albers."),
                                 tags$li("If variance based on a simple random sample is desired (or if
                                 coordinates are not available), the design stratum should be provided
                                 to better estimate variance."),
                                 tags$li("If change analysis is intended, all desired years of data must be
                                 contained in one file, with a single variable that identifies
                                         the individual years or survey cycles included.")
                               )),
               bsCollapsePanel(title = h4(strong("Run Population Estimates")),
               tags$ol(
                 tags$li("Select the type of analysis (categorical or continuous)."),
                 tags$li("If year or design cycle variable was selected on the Prepare Data for
                 Analysis tab, select year or cycle of interest."),
                 tags$li("For continuous analysis, select either CDFs (cumulative distribution
                         functions), percentiles, means, or totals."),
                 tags$li("Note that if data are missing for continuous variables,
                         those sites are ignored in analysis."),
                 tags$li("Click on the Run/Refresh Estimates button. Depending on the number of
                         responses, subpopulations, and type of analysis, it may take a few seconds
                         to several minutes."),
                 tags$li("If desired, download results to a comma-delimited file by clicking
                         the Save Results button.")
               )),
               bsCollapsePanel(title = h4(strong("Run Change Analysis")),
               tags$ol(
                 tags$li("First select the two years (or sets of years) to compare."),
                 tags$li("Select type of data to analyze (categorical or continuous)."),
                 tags$li("If continuous data are selected, select parameter on which to test
                         for differences (mean or median)."),
                 # tags$li("If repeated visits to sites are included in dataset across years or
                 #         cycles, check box. If selected, note that site ID variable selected
                 #         must contain the same value for both years or cycles of data."),
                 tags$li("Click on the Run/Refresh Estimates button. Depending on the number of
                 responses, subpopulations, and type of analysis, it may take a few
                         seconds to several minutes."),
                 tags$li("If any data are changed in the Prepare Data for Analysis tab, years
                         must be re-selected before running analysis.")
               )),

               bsCollapsePanel(title = h4(strong("Plot Categorical Estimates")),
                  tags$ol(
                    tags$li("Either run population estimates on categorical data either within the
                            app or import results into the app."),
                    tags$li("Variables in dataset must match those expected as output from
                            spsurvey::cat_analysis function:", strong("Type, Subpopulation, Indicator,
                            Category, Estimate.P, StdError.P, LCB95Pct.P, UCB95Pct.P, Estimate.U,
                            StdError.U,	LCB95Pct.U,	UCB95Pct.U")),
                    tags$li("Select either proportion or unit estimates to plot from Estimate Type."),
                    tags$li("Select Category values that represent Good, Fair, Poor, Not Assessed,
                            and Other condition classes. More than one value per condition class
                            in the dataset may be selected. For example, if one response uses
                            Good/Fair/Poor and another used At or Below Benchmark/Above Benchmark,
                            both Good and At or Below Benchmark can be selected."),
                    tags$li(strong("Optional:"), "add plot title and define resource type/unit
                            (i.e., stream length, number of lakes, coastal or wetland area)"),
                    tags$li("Click the Plot/Refresh Button to create plots."),
                    tags$li("From the menus on the right-hand side of the page, select the
                            Indicator of interest, and then the Subpopulation Group. Then
                            select the Subpopulation of interest. The upper plot shows the
                            individual subpopulation and the lower plot show a particular
                            condition class across all subpopulations."),
                    tags$li("To show confidence bound values, click the box above the main
                            and/or subpopulation plots."),
                    tags$li("The default order of the subpopulations in the lower plot is
                            alphabetical, but to sort by the estimate of the", em("Good"),
                            "class,
                            click the box for", em("Sort Subpopulations by Good Condition"),"."),
                    tags$li("Select", em("Download Estimate Plot"), "or",
                    em("Download Subpopulation Plot"), "to
                            save a .png file of the output. ")
                    )),
               bsCollapsePanel(title = h4(strong("Plot Continuous Estimates")),
                   tags$ol(
                     tags$li("Either run population estimates on continuous data to obtain
                             CDF estimates within the app, or import results into the app."),
                     tags$li("Variables in dataset must match those expected as output from
                spsurvey::cont_analysis function:", strong("Type, Subpopulation, Indicator,
                Value, Estimate.P, StdError.P, LCB95Pct.P, UCB95Pct.P, Estimate.U,
                StdError.U,	LCB95Pct.U,	UCB95Pct.U")),
                     tags$li("Select either proportion or unit estimates to plot
                             from Estimate Type."),
                     tags$li(strong("Optional:"), "Add plot title, indicator units,
                             and define resource type/unit."),
                     tags$li("Click the Plot Continuous Estimates button."),
                     tags$li("Select indicator from dropdown, then select a subpopulation group. Add
                             or remove subpopulations to the plot from the",
                             em("Add/Remove Subpopulations"),
                             "dropdown."),
                     tags$li(strong("Optional:"), "Add an Indicator Threshold, add confidence bands, and/or
                             change the x-axis to log10 scale. Be aware that if you have values
                             that are below or equal to zero, points will be excluded from the
                             plot if the", em("Log Scale X-Axis"), "option is selected.")
                     )),
               br(),
               p('Contact Karen Blocksom at blocksom.karen@epa.gov with questions or feedback.'),
               h3('Disclaimer'),
               p('The United States Environmental Protection Agency (EPA) GitHub project code is provided
                 on an "as is" basis and the user assumes responsibility for its use.  EPA has relinquished
                 control of the information and no longer has responsibility to protect the integrity,
                 confidentiality, or availability of the information.  Any reference to specific commercial
                 products, processes, or services by service mark, trademark, manufacturer, or otherwise,
                 does not constitute or imply their endorsement, recommendation or favoring by EPA.  The EPA
                 seal and logo shall not be used in any manner to imply endorsement of any commercial product
                 or activity by EPA or the United States Government.')),

# Prepare Data ------------------------------------------------------------

      # Panel to import and select data to analyze
      tabPanel(title='Prepare Data for Analysis',value="prepdata",

          fluidRow(
            # Input: Select a file ---
            column(3,
                add_busy_spinner(spin='fading-circle', position='full-page'),
                checkboxInput("websource", 'Input file from URL instead of local directory', FALSE),
                # Read in file from local computer
                conditionalPanel(condition="input.websource == false",
                  fileInput(inputId='file1', buttonLabel='Browse...',
                      label='Select a delimited file for analysis',
                      multiple=FALSE, accept=c('text/csv','text/comma-separated-values,text/plain','.csv')
                      )),
                # Read in file from website URL - need to add a button to signal it should start uploading
                conditionalPanel(condition="input.websource == true",
                                 textInput("urlfile", "Paste or enter full URL here."),
                                 actionButton("urlbtn", "Load file from URL")),
                # Horizontal line ----
                tags$hr(),
                # Input: checkbox if file has header, default to TRUE ----
                checkboxInput('header','Header',TRUE),
                # Input: Select delimiter ----
                radioButtons("sep","Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ","),

                # Horizontal line
                tags$hr(),

                # Input: Select number of rows to display
                radioButtons("disp","Display",
                             choices = c(Head = 'head',
                                         All = 'all'),
                             selected='head'),
                checkboxInput("subcheck","Subset data using a single categorical variable", FALSE),
                conditionalPanel(condition="input.subcheck == true",
                                 selectizeInput('subvar', "Select variable to use for subsetting", choices=NULL, multiple=FALSE),
                                 selectizeInput("subcat","Select one or more categories by which to subset data", choices=NULL,
                                                multiple=TRUE))),

            # Provide dropdown menus to allow user to select site, weight, and response variables from those in the imported dataset
            column(3,
              shiny::selectizeInput("siteVar", label="Select site variable",
                                    choices=NULL, multiple=FALSE) %>%
                helper(type = "inline", icon = 'exclamation', colour='#B72E16',
                       title = "Site variable selection",
                       content = paste("Select a site variable. If multiple years and resampled
                                       sites are included in the dataset, be sure the site
                                       variable has the same value across years."),
                       size = "s", easyClose = TRUE, fade = TRUE),
              selectizeInput("weightVar","Select weight variable", choices=NULL, multiple=FALSE),
              selectizeInput("respVar","Select up to 10 response variables - must all be either categorical or numeric",
                             choices=NULL, multiple=TRUE),
              checkboxInput('chboxYear', 'Check box if performing change analysis or need to subset data by year
                            or cycle for population estimates'),
              conditionalPanel(condition="input.chboxYear == true",
                               selectizeInput("yearVar","Select year variable",
                             choices=NULL, multiple=FALSE)),

              checkboxInput('natpop','Calculate overall (all sites) estimates?', TRUE),
              checkboxInput('subpop', 'Calculate estimates for subpopulations?', FALSE),
              # If national estimates box is NOT checked, show subpopulation dropdown list
              conditionalPanel(condition = 'input.subpop == true',
                               selectizeInput("subpopVar","Select up to 10 subpopulation variables",
                                              choices=NULL, multiple=TRUE)),

              h5(p(strong("If ANY changes have been made to your choices, you MUST click the button to prepare data for analysis again!"), style="color:#B72E16"))

            ),
            # Set up type of variance to use in estimates: local or SRS
            column(3,
                   radioButtons('locvar',"Type of variance estimate to use (select one)",
                                choices = c('Local neighborhood variance (recommended, used for NARS,
                                            requires site coordinates)' = 'local',
                                            'Simple Random Sample (requires stratum but not site
                                            coordinates)' = 'SRS'),
                                select = 'local'),
                   # If local, user must select x and y coordinates and convert to Albers if in lat/long
                   conditionalPanel(condition = "input.locvar == 'local'",
                                    selectizeInput("coordxVar","Select the X coordinate variable (or longitude)
                                                   (required only for local neighborhood variance)",
                                                   choices=NULL, multiple=FALSE),
                                    selectizeInput("coordyVar","Select the Y coordinate variable (or latitude)
                                                   (required only for local neighborhood variance)",
                                                   choices=NULL, multiple=FALSE)

                                                      ),

                   # Select stratum if Simple Random Sample
                   selectizeInput("stratumVar","Select a categorical stratum variable if desired. May be used for either variance type.",
                                  choices=NULL, multiple=FALSE)
            )
         ),
          # Press button to subset data for analysis - must click here first
         column(4, actionButton("resetBtn", "Click to revert back to full dataset or change data display")),
         column(4, actionButton("subsetBtn", strong("Click HERE to prepare data for analysis"), style="color:#B72E16"), offset=2),
         hr(),
         br(),

          # Show a table of the data
          h4("Data for Analysis"),
          DT::dataTableOutput("contents")

      ),

# Run Population Estimates ------------------------------------------------


      # Tab to run population estimates
      tabPanel(title="Run Population Estimates",value="runest",
          fluidRow(
             column(3,
                    # add_busy_bar(color="red", centered=TRUE),
               # User must select categorical or continuous variable analysis, depending on response variables selected
               radioButtons("atype","Type of Analysis (pick one)",
                            choices = c('Categorical (for character variables)' = 'categ',
                                        'Continuous (for numeric variables)' = 'contin'),
                            selected='categ'),
             # If continuous analysis selected, select whether CDFs or percentiles are desired.
             conditionalPanel(condition = "input.atype == 'contin'",
                              radioButtons("cdf_pct", "Show CDF, percentile, mean, or total results",
                                           choices = c(CDF = 'cdf', Percentiles = 'pct', Means = 'mean', Totals = 'total'),
                                           selected = 'pct')),
             conditionalPanel(condition="input.chboxYear==true",
                              selectizeInput('selYear', 'Select the year for analysis',
                                             choices=NULL, multiple=FALSE)),

             p("If the", strong("Run/Refresh Estimates"), "button is grayed out, return to the",
               strong("Prepare Data for Analysis"), "tab and click the button that says",
               strong("Click HERE to prepare data for analysis")),
             p("Note that if all values are very small, the results may appear as zeroes. Save
               and view output file to see the results with full digits."),
             # Once data are prepared, user needs to click to run estimates, or rerun estimates on refreshed data
             shinyjs::disabled(actionButton('runBtn', "Run/Refresh estimates")),
             hr(),
             # Click to download results into a comma-delimited file
             shinyjs::disabled(downloadButton("dwnldcsv","Save Results as .csv file"))),
             # Show results here
             column(8,
                    h4("If output is not as expected, be sure you chose the correct ",
                       strong("Type of Analysis"), "(Categorical or Continuous) for your data",
                       style="color:#225D9D"),
                    h4("Warnings"),
                    DT::dataTableOutput("warnest"),

                    h4("Analysis Output"),
                    DT::dataTableOutput("popest")
                    )
          )

      ),

# Run Change Analysis -----------------------------------------------------


      tabPanel(title="Run Change Analysis", value='change',
               fluidRow(
                 h4("  ", "If a different set of response variables from those
                 used in the population estimates is desired,
                    return to the", strong("Prepare Data for Analysis"),
                        "tab to re-select variables. Then click the button to
                        prepare data for analysis again.",
                    style="color:#225D9D"),
                 column(3,
                        # User must select years to compare
                        selectizeInput('chgYear1',"Select two years of data to compare in desired order",
                                       choices=NULL, multiple=TRUE),
                        radioButtons("chgCatCont", "Type of variables to analyze",
                                     choices = c(Categorical = 'chgCat', Continuous = 'chgCont'),
                                     select = 'chgCat'),
                        conditionalPanel(condition = "input.chgCatCont == 'chgCont'",
                                         radioButtons("testType", "Base test on mean or median",
                                                      choices = c(Mean='mean', Median='median'),
                                                      selected = 'mean')),

                 # Once data are prepared, user needs to click to run estimates, or rerun estimates on refreshed data
                 hr(),
                 p("If the", strong("Run/Refresh Estimates"), "button is grayed out, return to the",
                   strong("Prepare Data for Analysis"), "tab and click the button that says",
                   strong("Click HERE to prepare data for analysis")),
                 shinyjs::disabled(actionButton('chgBtn', "Run/Refresh estimates")),
                 hr(),
                 # Click to download results into a comma-delimited file
                 shinyjs::disabled(downloadButton("chgcsv","Save Change Results as .csv file"))),
                 column(8,
                         h4("Warnings"),
                        DT::dataTableOutput("warnchg"),

                        h4("Change Analysis Output"),
                        DT::dataTableOutput("changes")
                        )

               )

          ),

# Plot Data ---------------------------------------------------------------


      ####Categorical Plot UI####
      tabPanel(title="Plot Categorical Estimates",
               fluidRow(
                 sidebarPanel(
                   radioButtons(inputId="catinput",
                                label=strong("Choose Categorical Estimate Dataset to Use:"),
                                choices=c("Upload Estimate Data File", "Current Estimate Data"),
                                selected = "Upload Estimate Data File",
                                inline=FALSE),
                   uiOutput("catui"),
                   selectInput(inputId = "Estimate",
                               label = HTML("<b>Select Estimate Type</b>"),
                               choices = c("Proportion Estimates" = "P Estimates", "Unit Estimates" = "U Estimates"),
                               multiple = FALSE,
                               width = "300px")  %>%
                     #Estimate Type helper
                     helper(type = "inline",
                            title = "Estimate Type",
                            content = c("<b>Proportion Estimates:</b> Proportion of observations that
                                        belong to each level of the categorical variable.",
                                        "<b>Unit Estimates:</b> Total units (i.e. extent) that belong
                                        to each level of the categorical variable (total number
                                        (point resources), total line length (linear network), or
                                        total area (areal network))."),
                            size = "s", easyClose = TRUE, fade = TRUE),
                   tags$style(HTML(".shiny-output-error-validation {color: #ff0000; font-weight: bold;}")),
                   selectInput(inputId = "Good",
                               label = HTML("<b>Select <span style='color: #5796d1'>'Good'</span> Condition Classes</b>"),
                               choices = "",
                               selected = NULL,
                               multiple = TRUE,
                               width = "300px") %>%
                     #Condition Category helper
                     helper(type = "inline",
                            title = "Condition Category Color",
                            content = paste("The following inputs ask you to define the condition classes
                                        which will be used in the plots. You may use the same
                                        category for multiple conditions.", strong(em("For example, if
                                        your dataset
                                        contains some responses for which Good is the best condition
                                        and some for which Low is the best, you can select both for
                                        plotting. Only those applicable to a given response will be
                                        shown in the plot."))),
                            size = "s", easyClose = TRUE, fade = TRUE),
                   selectInput(inputId = "Fair",
                               label = HTML("<b>Select <span style='color: #EE9A00'>'Fair'</span> Condition Classes</b>"),
                               choices = "",
                               selected = NULL,
                               multiple = TRUE,
                               width = "300px"),
                   selectInput(inputId = "Poor",
                               label = HTML("<b>Select <span style='color: #f55b5b'>'Poor'</span> Condition Classes</b>"),
                               choices = "",
                               selected = NULL,
                               multiple = TRUE,
                               width = "300px"),
                   selectInput(inputId = "Not_Assessed",
                               label = HTML("<b>Select <span style='color: #c77505'>'Not Assessed'</span> Condition Classes</b>"),
                               choices = "",
                               selected = NULL,
                               multiple = TRUE,
                               width = "300px"),
                   selectInput(inputId = "Other",
                               label = HTML("<b>Select <span style='color: #d15fee'>'Other'</span> Condition Classes</b>"),
                               choices = "",
                               selected = NULL,
                               multiple = TRUE,
                               width = "300px"),
                   textInput("title", "Add a Plot Title", value = "", width = "300px", placeholder = "Optional"),
                   textInput("resource", "Define Resource Type/Unit", value = "",
                             width = "300px", placeholder = "Resource") %>%
                     #Resource Type helper
                     helper(type = "inline",
                            title = "Resource Type",
                            content = c("This input defines the plot axis label. Resource Type is
                                        the resource evaluated in your design (e.g., Stream Miles,
                                        Wetland Area, Coastal Area, Lakes)."),
                            size = "s", easyClose = TRUE, fade = TRUE),
                   actionButton("plotbtn", strong("Plot/Refresh Estimates"), icon=icon("chart-bar")),
                   br(),br(),
                   width = 3),
                 mainPanel(
                   conditionalPanel(condition="input.plotbtn",
                                    column(3,
                                           tags$head(tags$style(HTML("#Ind_Plot ~ .selectize-control.single .selectize-input {background-color: #FFD133;}"))),
                                           selectInput(inputId = "Ind_Plot",
                                                       label = HTML("<b>Select Indicator</b>"),
                                                       choices = "",
                                                       multiple = FALSE,
                                                       width = "200px") %>%
                                             #Indicator input helper
                                             helper(type = "inline",
                                                    title = "Indicator",
                                                    content = c("Select the indicator to display
                                                                categorical estimates by population."),
                                                    size = "s", easyClose = TRUE, fade = TRUE),

                                           #column(3,
                                           tags$head(tags$style(HTML("#Type_Plot ~ .selectize-control.single .selectize-input {background-color: #FFD133;}"))),
                                           selectInput(inputId = "Type_Plot",
                                                       label = HTML("<b>Select Subpopulation Group</b>"),
                                                       choices = NULL,
                                                       multiple = FALSE,
                                                       width = "200px") %>%
                                             #Subpop Group input helper
                                             helper(type = "inline",
                                                    title = "Subpopulation Group",
                                                    content = c("Select the Subpopulation group
                                                                to generate individual subpopulation
                                                                estimates and to use in the subpopulation
                                                                comparison plot."),
                                                    size = "s", easyClose = TRUE, fade = TRUE))),
                   column(8, offset = 2,
                          h3(strong(HTML("<center>Categorical Estimates by Population<center/>") %>%
                                      #Condition estimate helper
                                      helper(type = "inline",
                                             title = "Categorical Estimates",
                                             content = c("Cycle through populations to display
                                                         categorical estimates and download the plot,
                                                         if desired."),
                                             size = "s", easyClose = TRUE, fade = TRUE)))),
                   fluidRow(
                     column(3, offset=1,
                            conditionalPanel(condition="input.plotbtn",
                                             selectInput(inputId = "Tot_Pop",
                                                         label = HTML("<b>Select Subpopulation</b>"),
                                                         choices = NULL,
                                                         selected = NULL,
                                                         multiple = FALSE))),
                     column(3, offset=1,
                            conditionalPanel(condition="input.plotbtn",
                                             checkboxInput(inputId = "indconlim",
                                                           label= "Add Confidence Limit Values",
                                                           value = FALSE,
                                                           width = NULL)))),
                   fluidRow(
                     column(3, offset = 1,
                            conditionalPanel(condition="input.plotbtn",
                                             downloadButton("downloadPlot1", "Download Estimate Plot")))),

                   plotOutput("plot", width = "75%", height = "300px"),
                   br(), br(), br(),
                   column(8, offset = 2,
                          h3(strong(HTML("<center>Subpopulation Comparison<center/>") %>%
                                      #Subpopulation helper
                                      helper(type = "inline",
                                             title = "Subpopulation Comparison",
                                             content = c("Cycle through population groups to compare categorical estimates by condition and download plot, if desired. If there are no subpopulations, no subpopulation plots will appear."),
                                             size = "s", easyClose = TRUE, fade = TRUE)))),
                   fluidRow(
                     column(3, offset=1,
                            conditionalPanel(condition="input.plotbtn",
                                             # tags$head(tags$style(HTML("#Con_Plot ~ .selectize-control.single .selectize-input {border: 1px solid #33ACFF;}"))),
                                             selectInput(inputId = "Con_Plot",
                                                         label = HTML("<b>Select Condition</b>"),
                                                         choices = "",
                                                         multiple = FALSE))),
                     column(3, offset=1,
                            conditionalPanel(condition="input.plotbtn",
                                             checkboxInput(inputId = "goodsort",
                                                           label= HTML("Sort Subpopulations by <span style='color: #5796d1'>'Good'</span> Condition"),
                                                           value = FALSE,
                                                           width = NULL),
                                             checkboxInput(inputId = "subconlim",
                                                           label= "Add Confidence Limit Values",
                                                           value = FALSE,
                                                           width = NULL)))),
                   fluidRow(
                     column(3, offset=1,
                            conditionalPanel(condition="input.plotbtn",
                                             downloadButton('downloadPlot2', "Download Subpopulation Plot")))),
                   plotOutput("plot2", width = "75%"))
               )),
      ####Continuous Plot UI####
      tabPanel(title="Plot Continuous Estimates",
               sidebarPanel(
                 radioButtons(inputId="coninput",
                              label=strong("Choose Cumulative Distribution Function (CDF) Estimate Dataset to Use:"),
                              choices=c("Upload Estimate Data File", "Current Estimate Data"),
                              selected = "Upload Estimate Data File",
                              inline=FALSE),
                 uiOutput("conui"),
                            selectInput(inputId = "Estimate_CDF",
                                        label = HTML("<b>Select Estimate Type</b>"),
                                        choices = c("Proportion Estimates" = "P Estimates_CDF", "Unit Estimates" = "U Estimates_CDF"),
                                        multiple = FALSE,
                                        width = "300px")  %>%
                              #Estimate Type helper
                              helper(type = "inline",
                                     title = "Estimate Type",
                                     content = c("<b>Proportion Estimates:</b> Proportion of
                                                 observations that belong to each level of the
                                                 categorical variable.",
                                                 "<b>Unit Estimates:</b> Total units (i.e. extent)
                                                 that belong to each level of the categorical
                                                 variable (total number (point resources), total
                                                 line length (linear network), or total area
                                                 (areal network))."),
                                     size = "s", easyClose = TRUE, fade = TRUE),
                            textInput("title2", "Add a Plot Title", value = "", width = "300px", placeholder = "Optional"),
                            textInput("units", "Add Indicator Units", value = "", width = "300px", placeholder = "Optional"),
                            textInput("resource2", "Define Resource Type/Unit", value = "", width = "300px", placeholder = "Resource") %>%
                              #Resource Type helper
                              helper(type = "inline",
                                     title = "Resource Type",
                                     content = c("This input defines the plot axis label. Resource
                                                 Type is the resource evaluated in your design
                                                 (e.g., Stream Miles, Wetland Area, Coastline)."),
                                     size = "s", easyClose = TRUE, fade = TRUE),
                            actionButton("plotbtncon", strong("Plot Continuous Estimates"), icon=icon("chart-bar"))
                            ,width=4),#sidebarPanel
               mainPanel(
                 column(3, offset=1,
                        conditionalPanel(condition="input.plotbtncon",
                                         selectInput(inputId = "Ind_Con",
                                                     label = HTML("<b>Select Indicator</b>"),
                                                     choices = "",
                                                     multiple = FALSE)),
                        conditionalPanel(condition="input.plotbtncon",
                                         selectInput(inputId = "Pop_Con",
                                                     label = HTML("<b>Select Population</b>"),
                                                     choices = "",
                                                     multiple = FALSE)),
                        conditionalPanel(condition="input.plotbtncon",
                                         selectInput(inputId = "SubPop_Con",
                                                     label = HTML("<b>Add/Remove Subpopulations</b>"),
                                                     choices = "",
                                                     multiple = TRUE))),
                 fluidRow(
                   column(3, offset=1,
                          conditionalPanel(condition="input.plotbtncon",
                                           numericInput("Thresh", "Indicator Threshold (optional)",
                                                        value = NULL),
                                           checkboxInput(inputId = "conflim",
                                                         label="Add Confidence Limits",
                                                         value = FALSE,
                                                         width = NULL),
                                           checkboxInput(inputId = "log",
                                                         label="Log Scale X-Axis",
                                                         value = FALSE,
                                                         width = NULL)))),

                 column(8, offset = 2,
                        h3(strong(HTML("<center>CDF Estimates<center/>") %>%
                                    #CDF helper
                                    helper(type = "inline",
                                           title = "Cumulative Distribution Function (CDF)",
                                           content = c("A Cumulative Distribution Function calculates
                                                       the cumulative probability for a given value and
                                                       can be used to determine the probability that a
                                                       random observation that is taken from the
                                                       population will be less than or equal to a certain
                                                       value."),
                                           size = "s", easyClose = TRUE, fade = TRUE))),
                 hr(),
                 h4("NOTE: Plotting and downloading may take a while if there are multiple
                          subpopulations. PLEASE BE PATIENT.")),
                 fluidRow(
                   column(3,
                          conditionalPanel(condition="input.plotbtncon",
                                           downloadButton("download1", "Download CDF Plot")))),
                 plotOutput("CDFsubplot", width = "75%"),
                 br(), br(), br(),
                 column(8, offset = 2,
                        h3(strong(HTML("<center>Distribution of Estimates by Population<center/>") %>%
                                    #CDF helper
                                    helper(type = "inline",
                                           title = "Ridgeline Distribution Plot",
                                           content = c("A <b>Ridgeline Plot</b>, also known as a Joy Plot,
                                                       is used to visualize distributions of several groups.
                                                       Each group produces a density curve which overlaps
                                                       with each other to help visualize differences.",
                                                       "",
                                                       "Small vertical lines represent values.",
                                                       "Large vertical lines represent quartiles."),
                                           size = "s", easyClose = TRUE, fade = TRUE)))),
                 fluidRow(
                   column(3,
                          conditionalPanel(condition="input.plotbtncon",
                                           downloadButton("download2", "Download Distribution Plot")))),
                 br(),
                 plotOutput("Distplot", width = "75%")
               ))
   ),
# Site Footer ----
HTML(
  '</main>
      <footer class="footer" role="contentinfo">
      <div class="l-constrain">
        <img class="footer__epa-seal" src="https://www.epa.gov/themes/epa_theme/images/epa-seal.svg" alt="United States Environmental Protection Agency" height="100" width="100">
        <div class="footer__content contextual-region">
          <div class="footer__column">
            <h2>Discover.</h2>
            <ul class="menu menu--footer">
              <li class="menu__item">
                <a href="/accessibility" class="menu__link">Accessibility</a>
              </li>
              <!--li class="menu__item"><a href="#" class="menu__link">EPA Administrator</a></li-->
              <li class="menu__item">
                <a href="/planandbudget" class="menu__link">Budget &amp; Performance</a>
              </li>
              <li class="menu__item">
                <a href="/contracts" class="menu__link">Contracting</a>
              </li>
              <li class="menu__item">
                <a href="/home/wwwepagov-snapshots" class="menu__link">EPA www Web Snapshot</a>
              </li>
              <li class="menu__item">
                <a href="/grants" class="menu__link">Grants</a>
              </li>
              <li class="menu__item">
                <a href="/ocr/whistleblower-protections-epa-and-how-they-relate-non-disclosure-agreements-signed-epa-employees" class="menu__link">No FEAR Act Data</a>
              </li>
              <li class="menu__item">
                <a href="/web-policies-and-procedures/plain-writing" class="menu__link">Plain Writing</a>
              </li>
              <li class="menu__item">
                <a href="/privacy" class="menu__link">Privacy</a>
              </li>
              <li class="menu__item">
                <a href="/privacy/privacy-and-security-notice" class="menu__link">Privacy and Security Notice</a>
              </li>
            </ul>
          </div>
          <div class="footer__column">
            <h2>Connect.</h2>
            <ul class="menu menu--footer">
              <li class="menu__item">
                <a href="https://www.data.gov/" class="menu__link">Data.gov</a>
              </li>
              <li class="menu__item">
                <a href="/office-inspector-general/about-epas-office-inspector-general" class="menu__link">Inspector General</a>
              </li>
              <li class="menu__item">
                <a href="/careers" class="menu__link">Jobs</a>
              </li>
              <li class="menu__item">
                <a href="/newsroom" class="menu__link">Newsroom</a>
              </li>
              <li class="menu__item">
                <a href="/data" class="menu__link">Open Government</a>
              </li>
              <li class="menu__item">
                <a href="https://www.regulations.gov/" class="menu__link">Regulations.gov</a>
              </li>
              <li class="menu__item">
                <a href="/newsroom/email-subscriptions-epa-news-releases" class="menu__link">Subscribe</a>
              </li>
              <li class="menu__item">
                <a href="https://www.usa.gov/" class="menu__link">USA.gov</a>
              </li>
              <li class="menu__item">
                <a href="https://www.whitehouse.gov/" class="menu__link">White House</a>
              </li>
            </ul>
          </div>
          <div class="footer__column">
            <h2>Ask.</h2>
            <ul class="menu menu--footer">
              <li class="menu__item">
                <a href="/home/forms/contact-epa" class="menu__link">Contact EPA</a>
              </li>
              <li class="menu__item">
                <a href="/web-policies-and-procedures/epa-disclaimers" class="menu__link">EPA Disclaimers</a>
              </li>
              <li class="menu__item">
                <a href="/aboutepa/epa-hotlines" class="menu__link">Hotlines</a>
              </li>
              <li class="menu__item">
                <a href="/foia" class="menu__link">FOIA Requests</a>
              </li>
              <li class="menu__item">
                <a href="/home/frequent-questions-specific-epa-programstopics" class="menu__link">Frequent Questions</a>
              </li>
            </ul>
            <h2>Follow.</h2>
            <ul class="menu menu--social">
              <li class="menu__item">
                <a class="menu__link" aria-label="EPA’s Facebook" href="https://www.facebook.com/EPA">
                  <!-- svg class="icon icon--social" aria-hidden="true" -->
                  <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="facebook-square" xmlns="http://www.w3.org/2000/svg">
                    <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#facebook-square"></use-->
                    <path fill="currentcolor" d="M400 32H48A48 48 0 000 80v352a48 48 0 0048 48h137.25V327.69h-63V256h63v-54.64c0-62.15 37-96.48 93.67-96.48 27.14 0 55.52 4.84 55.52 4.84v61h-31.27c-30.81 0-40.42 19.12-40.42 38.73V256h68.78l-11 71.69h-57.78V480H400a48 48 0 0048-48V80a48 48 0 00-48-48z"></path>
                  </svg> 
                  <span class="usa-tag external-link__tag" title="Exit EPA Website">
                    <span aria-hidden="true">Exit</span>
                    <span class="u-visually-hidden"> Exit EPA Website</span>
                  </span>
                </a>
              </li>
              <li class="menu__item">
                <a class="menu__link" aria-label="EPA’s Twitter" href="https://twitter.com/epa">
                  <!-- svg class="icon icon--social" aria-hidden="true" -->
                  <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="twitter-square" xmlns="http://www.w3.org/2000/svg">
                    <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#twitter-square"></use -->
                    <path fill="currentcolor" d="M400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48zm-48.9 158.8c.2 2.8.2 5.7.2 8.5 0 86.7-66 186.6-186.6 186.6-37.2 0-71.7-10.8-100.7-29.4 5.3.6 10.4.8 15.8.8 30.7 0 58.9-10.4 81.4-28-28.8-.6-53-19.5-61.3-45.5 10.1 1.5 19.2 1.5 29.6-1.2-30-6.1-52.5-32.5-52.5-64.4v-.8c8.7 4.9 18.9 7.9 29.6 8.3a65.447 65.447 0 01-29.2-54.6c0-12.2 3.2-23.4 8.9-33.1 32.3 39.8 80.8 65.8 135.2 68.6-9.3-44.5 24-80.6 64-80.6 18.9 0 35.9 7.9 47.9 20.7 14.8-2.8 29-8.3 41.6-15.8-4.9 15.2-15.2 28-28.8 36.1 13.2-1.4 26-5.1 37.8-10.2-8.9 13.1-20.1 24.7-32.9 34z"></path>
                  </svg>
                  <span class="usa-tag external-link__tag" title="Exit EPA Website">
                    <span aria-hidden="true">Exit</span>
                    <span class="u-visually-hidden"> Exit EPA Website</span>
                  </span>
                </a>
              </li>
              <li class="menu__item">
                <a class="menu__link" aria-label="EPA’s Youtube" href="https://www.youtube.com/user/USEPAgov">
                  <!-- svg class="icon icon--social" aria-hidden="true" -->
                  <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="youtube-square" xmlns="http://www.w3.org/2000/svg">
                    <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#youtube-square"></use -->
                    <path fill="currentcolor" d="M186.8 202.1l95.2 54.1-95.2 54.1V202.1zM448 80v352c0 26.5-21.5 48-48 48H48c-26.5 0-48-21.5-48-48V80c0-26.5 21.5-48 48-48h352c26.5 0 48 21.5 48 48zm-42 176.3s0-59.6-7.6-88.2c-4.2-15.8-16.5-28.2-32.2-32.4C337.9 128 224 128 224 128s-113.9 0-142.2 7.7c-15.7 4.2-28 16.6-32.2 32.4-7.6 28.5-7.6 88.2-7.6 88.2s0 59.6 7.6 88.2c4.2 15.8 16.5 27.7 32.2 31.9C110.1 384 224 384 224 384s113.9 0 142.2-7.7c15.7-4.2 28-16.1 32.2-31.9 7.6-28.5 7.6-88.1 7.6-88.1z"></path>
                  </svg>
                  <span class="usa-tag external-link__tag" title="Exit EPA Website">
                    <span aria-hidden="true">Exit</span>
                    <span class="u-visually-hidden"> Exit EPA Website</span>
                  </span>
                </a>
              </li>
              <li class="menu__item">
                <a class="menu__link" aria-label="EPA’s Flickr" href="https://www.flickr.com/photos/usepagov">
                  <!-- svg class="icon icon--social" aria-hidden="true" -->
                  <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="flickr-square" xmlns="http://www.w3.org/2000/svg">
                    <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#flickr-square"></use -->
                    <path fill="currentcolor" d="M400 32H48C21.5 32 0 53.5 0 80v352c0 26.5 21.5 48 48 48h352c26.5 0 48-21.5 48-48V80c0-26.5-21.5-48-48-48zM144.5 319c-35.1 0-63.5-28.4-63.5-63.5s28.4-63.5 63.5-63.5 63.5 28.4 63.5 63.5-28.4 63.5-63.5 63.5zm159 0c-35.1 0-63.5-28.4-63.5-63.5s28.4-63.5 63.5-63.5 63.5 28.4 63.5 63.5-28.4 63.5-63.5 63.5z"></path>
                  </svg>
                  <span class="usa-tag external-link__tag" title="Exit EPA Website">
                    <span aria-hidden="true">Exit</span>
                    <span class="u-visually-hidden"> Exit EPA Website</span>
                  </span>
                </a>
              </li>
              <li class="menu__item">
                <a class="menu__link" aria-label="EPA’s Instagram" href="https://www.instagram.com/epagov">
                  <!-- svg class="icon icon--social" aria-hidden="true" -->
                  <svg class="icon icon--social" aria-hidden="true" viewBox="0 0 448 512" id="instagram-square" xmlns="http://www.w3.org/2000/svg">
                    <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#instagram-square"></use -->
                    <path fill="currentcolor" xmlns="http://www.w3.org/2000/svg" d="M224 202.66A53.34 53.34 0 10277.36 256 53.38 53.38 0 00224 202.66zm124.71-41a54 54 0 00-30.41-30.41c-21-8.29-71-6.43-94.3-6.43s-73.25-1.93-94.31 6.43a54 54 0 00-30.41 30.41c-8.28 21-6.43 71.05-6.43 94.33s-1.85 73.27 6.47 94.34a54 54 0 0030.41 30.41c21 8.29 71 6.43 94.31 6.43s73.24 1.93 94.3-6.43a54 54 0 0030.41-30.41c8.35-21 6.43-71.05 6.43-94.33s1.92-73.26-6.43-94.33zM224 338a82 82 0 1182-82 81.9 81.9 0 01-82 82zm85.38-148.3a19.14 19.14 0 1119.13-19.14 19.1 19.1 0 01-19.09 19.18zM400 32H48A48 48 0 000 80v352a48 48 0 0048 48h352a48 48 0 0048-48V80a48 48 0 00-48-48zm-17.12 290c-1.29 25.63-7.14 48.34-25.85 67s-41.4 24.63-67 25.85c-26.41 1.49-105.59 1.49-132 0-25.63-1.29-48.26-7.15-67-25.85s-24.63-41.42-25.85-67c-1.49-26.42-1.49-105.61 0-132 1.29-25.63 7.07-48.34 25.85-67s41.47-24.56 67-25.78c26.41-1.49 105.59-1.49 132 0 25.63 1.29 48.33 7.15 67 25.85s24.63 41.42 25.85 67.05c1.49 26.32 1.49 105.44 0 131.88z"></path>
                  </svg>
                  <span class="usa-tag external-link__tag" title="Exit EPA Website">
                    <span aria-hidden="true">Exit</span>
                    <span class="u-visually-hidden"> Exit EPA Website</span>
                  </span>
                </a>
              </li>
            </ul>
            <p class="footer__last-updated">
              Last updated on March 30, 2022
            </p>
          </div>
        </div>
      </div>
    </footer>
    <a href="#" class="back-to-top" title="">
      <svg class="back-to-top__icon" role="img" aria-label="">
      <svg class="back-to-top__icon" role="img" aria-label="" viewBox="0 0 19 12" id="arrow" xmlns="http://www.w3.org/2000/svg">
        <!-- use xlink:href="https://www.epa.gov/themes/epa_theme/images/sprite.artifact.svg#arrow"></use -->
        <path fill="currentColor" d="M2.3 12l7.5-7.5 7.5 7.5 2.3-2.3L9.9 0 .2 9.7 2.5 12z"></path>
      </svg>
    </a>'
)
) # END fluidPage

# Begin Server ----
server <- function(input, output, session) {
  observe_helpers()

# Read in Data for Preparation --------------------------------------------


  # Read in data file as selected
  dataIn <- reactive({
    if(input$websource==FALSE){
      file1 <- input$file1
      req(file1)
      df <- read.table(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     stringsAsFactors=F)

    }else{
      if(input$urlbtn>0){
        df <- read.table(url(input$urlfile),
                         header = input$header,
                         sep = input$sep,
                         stringsAsFactors=F)
      }
    }

    df

  })

  # Need code to show initial version of dataIn() before subsetting or pressing the button to reset

  observeEvent(input$subsetBtn, {
    shinyjs::disable('dwnldcsv')
    shinyjs::enable('chgBtn')
    shinyjs::enable('runBtn')
    shinyjs::disable('chgcsv')
    if(input$disp == 'head'){
      output$contents <- DT::renderDataTable({head(dataOut())}, options = list(digits=5, rownames=F,
                                             scrollX=TRUE, scrollY=TRUE))
    }else{
      output$contents <- DT::renderDataTable({dataOut()}, options = list(digits=5, rownames=F,
                                     scrollX=TRUE, scrollY=TRUE))
    }
  })

  observeEvent(input$resetBtn, {
    if(input$disp == 'head'){
      output$contents <- DT::renderDataTable({head(dataIn())},
                                             options = list(digits=5, rownames=F,
                                                            scrollX=TRUE, scrollY=TRUE))
    }else{
      output$contents <- DT::renderDataTable({dataIn()}, options = list(digits=5, rownames=F,
                                                                        scrollX=TRUE, scrollY=TRUE))
    }
  })


  # Use current dataset to refresh dropdown list of variables.
  observe({
    vars <- colnames(dataIn())
    updateSelectizeInput(session, "weightVar", "Select weight variable", choices=vars)

    updateSelectizeInput(session, 'respVar', 'Select up to 10 response variables - All must be either categorical or numeric',
                         choices=vars, selected = NULL,
                         options = list(maxItems=10))
    updateSelectizeInput(session, 'coordxVar', "Select the X coordinate variable (or longitude) \n(required only
                         for local neighborhood variance)",
                         choices=vars, selected = NULL)
    updateSelectizeInput(session, 'coordyVar', "Select the Y coordinate variable (or latitude) \n(required only
                         for local neighborhood variance)",
                         choices=vars, selected = NULL)
    updateSelectizeInput(session, 'siteVar', 'Select site variable', choices=vars)
    updateSelectizeInput(session, 'subpopVar', 'Select up to 10 subpopulation variables \n(required if not
                         national estimates only)', choices=vars, selected=NULL,
                         options = list(maxItems=10))
    updateSelectizeInput(session, "stratumVar", "Select a categorical stratum variable if desired. May be used for either variance type.",
                         choices=c('None', vars), selected='None')
    updateSelectizeInput(session, "yearVar","Select year variable",
                         choices=c('', vars))

    updateSelectizeInput(session, "subvar", choices=vars)

    updateSelectizeInput(session, 'szwtVar', choices=vars)
  })

  observeEvent(input$subvar,{

      catchoices <- as.character(sort(unique(dataIn()[[input$subvar]])))

      updateSelectizeInput(session, 'subcat', choices = catchoices, selected=NULL, server=TRUE)

  })


# After Prepare Data for Analysis Button Clicked --------------------------


  # Once subset button is clicked, validate selections to make sure any variable only occurs in set of selections
  dataOut <- eventReactive(input$subsetBtn,{
    if(input$subsetBtn > 0){
      if(input$chboxYear==TRUE){
        yearVName <- input$yearVar
      }else{
        yearVName <- NULL
      }
      if(input$subcheck==TRUE){
        subVName <- input$subvar
      }else{
        subVName <- NULL
      }

      # print(input$subpopVar)

    if(input$subpop==TRUE & is.null(input$subpopVar)){
      validate(!is.null(input$subpopVar), "Choose a subpopulation variable or
               uncheck the subpopulation box!")
    }

      if(input$subpop == TRUE & !is.null(input$subpopVar)){
        # Use function below to validate input variables as the appropriate type and to make sure the selections do not overlap
        validate(
          need(input$subpopVar %nin% c(input$siteVar,input$weightVar,input$respVar),
               "Subpopulation variable(s) cannot overlap with other variable selections."),
          need(input$respVar %nin% c(input$siteVar,input$subpopVar,input$weightVar),
               "Response variable(s) cannot overlap with other variable selections."),
          need(input$weightVar %nin% c(input$siteVar,input$subpopVar,input$respVar),
               "Weight variable cannot overlap with other variable selections."),
          need(input$siteVar %nin% c(input$respVar,input$subpopVar,input$weightVar),
               'Site variable cannot overlap with other variable selections.')
          )
          if(input$chboxYear==TRUE){
             validate(
               need(yearVName %nin% c(input$respVar,input$subpopVar,input$weightVar,input$siteVar),
                    "Year variable cannot overlap with other variable selections")
             )
          }
          # If local variance selected, make sure x and y coordinate variables do not overlap with any already selected
          if(input$locvar == 'local'){
            validate(
              need(input$coordxVar %nin% c(input$siteVar,input$coordyVar,input$respVar,input$subpopVar,input$weightVar),
                 "X-coordinate variable cannot overlap with other variable selections."),
              need(input$coordyVar %nin% c(input$siteVar,input$coordxVar,input$respVar,input$subpopVar,input$weightVar),
                 "Y-coordinate variable cannot overlap with other variable selections.")
              )
            if(input$chboxYear==TRUE){
              validate(
                need(input$yearVar %nin% c(input$coordxVar, input$coordyVar),
                     "Year variable cannot overlap with other variable selections")
              )
            }


          # If local variance not used, need stratum variable but not coordinates
          }else{
            # validate variable for stratum to make sure it does not overlap with other variables selected
            validate(
              need(input$stratumVar %nin% c(input$siteVar,input$respVar,input$subpopVar,
                                            input$weightVar,yearVName, subVName),
                   "Stratum variable cannot overlap with other variable selections.")
              )
            if(input$chboxYear==TRUE){
              validate(
                need(yearVName %nin% c(input$stratumVar),
                     "Year variable cannot overlap with other variable selections")
              )
            }
          }



          df1 <- dataIn()

      # If All Sites only estimates selected, changes selection of data for analysis
      }else{
        # Use function below to validate input variables as the appropriate type and to make sure the selections do not overlap
        validate(
          need(input$respVar %nin% c(input$siteVar,input$weightVar),
               "Response variable(s) cannot overlap with other variable selections."),
          need(input$weightVar %nin% c(input$siteVar,input$respVar),
               "Weight variable cannot overlap with other variable selections."),
          need(input$siteVar %nin% c(input$respVar,input$weightVar),
               'Site variable cannot overlap with other variable selections.')
        )
        if(input$chboxYear==TRUE){
          validate(
            need(input$yearVar %nin% c(input$siteVar, input$weightVar, input$respVar),
                 "Year variable cannot overlap with other variable selections")
          )
        }
        # if local neighborhood variance used here, make sure coordinates variables do not overlap with other selections
        if(input$locvar == 'local'){
          validate(
            need(input$coordxVar %nin% c(input$siteVar,input$coordyVar,input$respVar,input$weightVar),
                 "X-coordinate variable cannot overlap with other variable selections."),
            need(input$coordyVar %nin% c(input$siteVar,input$coordxVar,input$respVar,input$weightVar),
                 "Y-coordinate variable cannot overlap with other variable selections.")
          )
          validate(
            need(input$stratumVar %nin% c(input$siteVar,input$respVar,input$weightVar),
                 "Stratum variable cannot overlap with other variable selections. If no stratum variable, select 'None'")
          )
          if(input$chboxYear==TRUE){
            validate(
              need(input$yearVar %nin% c(input$coordxVar, input$coordyVar),
                   "Year variable cannot overlap with other variable selections")
            )
          }

          # Subset the data to selected variables if year selected


        # if stratum selected, make sure stratum does not overlap with other variable selections
        }else{
          validate(
            need(input$stratumVar %nin% c(input$siteVar,input$respVar,input$weightVar),
                 "Stratum variable cannot overlap with other variable selections.")
          )
          if(input$chboxYear==TRUE){
            validate(
              need(input$yearVar %nin% c(input$stratumVar),
                   "Year variable cannot overlap with other variable selections")
            )
          }

        }

      }
      df1 <- dataIn()
      # Drop any rows with weights missing or 0
      df1 <- subset(df1, !is.na(eval(as.name(input$weightVar))) & eval(as.name(input$weightVar))>0)
      if(input$subcheck==TRUE){
        df1 <- subset(df1, eval(as.name(input$subvar)) %in% input$subcat)
      }
      # Look for missing values among coordinates and weights only - response and subpopulation variables can have missing values
      # If local neighborhood variance not used, these values are null (not in df1) and thus return TRUE.
      validate(
        need(!any(is.na(df1[,input$coordxVar])), "Non-numeric or missing values for x coordinates."),
        need(!any(is.na(df1[,input$coordyVar])), "Non-numeric or missing values for y coordinates.")
      )
      df1

    # This is what shows up before the subset button is clicked
    }else{
      df1 <- dataIn()
    }
  }

  )


  # Allow user to select specific year of interest for analysis
  observe({
                 ychoices <- as.character(sort(unique(dataOut()[[input$yearVar]])))

                 updateSelectizeInput(session, 'selYear', 'Select the year for analysis',
                                      choices = ychoices, selected=NULL)


                 updateSelectizeInput(session, 'chgYear1', "Select two years or design cycles of data to compare",
                                      choices = ychoices, selected=NULL, options = list(maxItems=2))


                 }
               )


# Run Change Estimates Code -----------------------------------------------


  # Change estimate code
  chgEst <- eventReactive(input$chgBtn,{

    if(exists("warn_df") && is.data.frame(get("warn_df"))){
      rm("warn_df", envir=.GlobalEnv)
    }

      chgIn <- dataOut()

      chgIn <- subset(chgIn, eval(as.name(input$yearVar)) %in% input$chgYear1)

      # Check for duplicate rows for siteID
      freqSiteChg <- as.data.frame(table(siteID = chgIn[,input$siteVar],Year = chgIn[,input$yearVar]))

      validate(
        need(nrow(subset(freqSiteChg, Freq>1))==0,
             paste("There are", nrow(subset(freqSiteChg, Freq>1)),
                   "duplicated sites in this dataset within years or cycles.
                   Only one row per site-design cycle combination is permitted in the input data."))
      )
      if(input$chgCatCont=='chgCat'){

        validate(
          need(all('character' %in% lapply(chgIn[,input$respVar], class)),
               'At least one response variable is numeric data. Do you mean to run CONTINUOUS analysis?')
        )
      }else{

        validate(
          need(all('numeric' %in% lapply(chgIn[,input$respVar], class)),
               'At least one response variable is character. Do you mean to run CATEGORICAL analysis?')
        )
      }

      # Need to order by siteID, yearVar
      chgIn <- chgIn[order(chgIn[,input$yearVar],chgIn[,input$siteVar]),]

      # print(input$chgYear1[[1]])
      # print(input$chgYear1[[2]])
      surveyID <- input$yearVar
      survey_names <- c(input$chgYear1[[1]], input$chgYear1[[2]])
      validate(
        need(length(survey_names)==2,
             paste("Select years or design cycles to compare."))
      )

      if(input$natpop==TRUE & input$subpop==TRUE){
        all_sites <- TRUE
        # chgIn$All_Sites <- factor('All Sites')
      }else{
        all_sites <- FALSE
      }

      if(input$chgCatCont == 'chgCat'){
        vars_cat <- input$respVar
      }else{
        vars_cont <- input$respVar
      }

      if(input$locvar == 'local'){
        vartype <- 'Local'
      }else{
        vartype <- 'SRS'
      }

      if(input$subpop == FALSE){
        subpops.in <- NULL
      }else{
        subpops.in <- input$subpopVar
      }

      if(input$stratumVar=='None'){
        stratum.in <- NULL
      }else{
        stratum.in <- input$stratumVar
      }

      if(is.null(input$coordxVar)){
        xcoord.in <- NULL
      }else{
        xcoord.in <- input$coordxVar
      }

      if(is.null(input$coordyVar)){
        ycoord.in <- NULL
      }else{
        ycoord.in <- input$coordyVar
      }

      if(input$chgCatCont == 'chgCont'){
        if(input$testType == 'mean'){
          ttype <- 'mean'
        }else{
          ttype <- 'median'
        }
      }

       #revisitWgt <- FALSE # NOT SURE WHAT THIS SHOULD BE SO ASSUME DEFAULT
       show_modal_spinner(spin = 'flower', text = 'This might take a while...please wait.')

        # if(input$repeatBox==TRUE){
          if(input$chgCatCont == 'chgCat'){
            chgOut <- change_analysis(dframe = chgIn, vars_cat = input$respVar,
                                      subpops=subpops.in, surveyID = surveyID,
                                      survey_names = survey_names,
                                      siteID = input$siteVar, weight = input$weightVar,
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      stratumID = stratum.in,
                                      vartype = vartype, All_Sites = all_sites)
          }else{
            chgOut <- change_analysis(dframe = chgIn, vars_cont = input$respVar, test = ttype,
                                      subpops=subpops.in, surveyID = surveyID,
                                      survey_names = survey_names,
                                      siteID = input$siteVar, weight = input$weightVar,
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      stratumID = stratum.in,
                                      vartype = vartype, All_Sites = all_sites)
          }

      remove_modal_spinner()

    if(input$chgCatCont == 'chgCat'){
      chgOut.1 <- chgOut$catsum

      # Order the Category values only if all values fall within those below using factors
      if(all(unique(chgOut.1$Category %in% c('Good','GOOD','Low','LOW',
                                                           'Fair','FAIR','MODERATE','Moderate',
                                                           'Poor','POOR','High','HIGH',
                                                           'Very High','VERY HIGH','Not Assessed')))){
            chgOut.1$Category <- factor(chgOut.1$Category,
                                                 levels=c('Good','GOOD','Low','LOW',
                                                          'Fair','FAIR','MODERATE','Moderate',
                                                          'Poor','POOR','High','HIGH',
                                                          'Very High','VERY HIGH','Not Assessed'),
                                                 ordered=TRUE)
            chgOut.1$Category <- droplevels(chgOut.1$Category)

            chgOut.1 <- chgOut.1[with(chgOut.1, order(Type, Subpopulation, Indicator, Category)),]
        }

    }else{
      if(input$testType == 'mean'){
        chgOut.1 <- chgOut$contsum_mean
      }else{
        chgOut.1 <- chgOut$contsum_median
      }
    }

    if(exists('warn_df') && ncol(warn_df)>1){
      outdf <- list(chgOut=chgOut.1, warndf=warn_df)
    }else{
      outdf <- list(chgOut=chgOut.1, warndf=data.frame(warnings='none'))
    }


  })
  # Use change output to create a table
  output$changes <- DT::renderDataTable({
    chgEst()[['chgOut']]
  }, options = list(scrollX=TRUE, scrollY=TRUE, rownames=F, searching=FALSE))

  output$warnchg <- DT::renderDataTable({
    chgEst()[['warndf']]
  }, options = list(scrollX=TRUE, scrollY=TRUE, rownames=F, searching=FALSE))


# Single Year Population Estimates ----------------------------------------


  # Calculate population estimates
  dataEst <- eventReactive(input$runBtn,{

    if(exists("warn_df") && is.data.frame(get("warn_df"))){
      rm("warn_df", envir=.GlobalEnv)
    }

    dfIn <- dataOut()

    if(input$natpop==TRUE & input$subpop==TRUE){
      all_sites <- TRUE
    }else{
      all_sites <- FALSE
    }

    if(input$chboxYear==TRUE){
      dfIn <- subset(dfIn, eval(as.name(input$yearVar)) == as.character(input$selYear))
    }

    # Check for duplicate rows for siteID
    freqSite <- as.data.frame(table(siteID=dfIn[,input$siteVar]))

    validate(
      need(nrow(subset(freqSite, Freq>1))==0,
           paste("There are", nrow(subset(freqSite, Freq>1)),"duplicated sites in this dataset.
                 Only one row per site permitted in the input data."))
    )

    # VALIDATION of variable types

    if(input$atype=='categ'){

      validate(
        need(all('character' %in% lapply(dfIn[,input$respVar], class)),
        'At least one response variable is numeric data. Do you mean to run CONTINUOUS analysis?')
      )
    }else{

      validate(
        need(all('numeric' %in% lapply(dfIn[,input$respVar], class)),
             'At least one response variable is character. Do you mean to run CATEGORICAL analysis?')
      )
    }

    show_modal_spinner(spin = 'flower', text = 'This might take a while...please wait.')

    # If categorical data, automatically reorder any response variables that are Good/Fair/Poor or
    # Low/Moderate/High (allow for all caps versions)
    if(input$atype=='categ'){
      for(i in 1:length(input$respVar)){
        if(all(unique(dfIn[,input$respVar[[i]]]) %in% c('Good','GOOD','Low','LOW',
                                                        'Fair','FAIR','MODERATE','Moderate',
                                                        'Poor','POOR','High','HIGH',
                                                        'Very High','VERY HIGH','Not Assessed'))){
          dfIn[,input$respVar[[i]]] <- factor(dfIn[,input$respVar[[i]]],
                                        levels=c('Good','GOOD','Low','LOW',
                                                 'Fair','FAIR','MODERATE','Moderate',
                                                 'Poor','POOR','High','HIGH',
                                                 'Very High','VERY HIGH','Not Assessed'),
                                              ordered=TRUE)
        }
      }
    }

      # Create vartype variable depending on option selected
      if(input$locvar == 'local'){
        vartype <- 'Local'
      }else{
        vartype <- 'SRS'
      }

      if(input$subpop == FALSE){
        subpops.in <- NULL
      }else{
        subpops.in <- input$subpopVar
      }

      if(input$stratumVar=='None'){
        stratum.in <- NULL
      }else{
        stratum.in <- input$stratumVar
      }

      if(is.null(input$coordxVar)){
        xcoord.in <- NULL
      }else{
        xcoord.in <- input$coordxVar
      }

      if(is.null(input$coordyVar)){
        ycoord.in <- NULL
      }else{
        ycoord.in <- input$coordyVar
      }

       # User selected categorical analysis, set up cat.analysis function depending on previous selections
      if(input$atype=='categ'){
            estOut <- cat_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in,
                                   vars=input$respVar, weight = input$weightVar,
                                   xcoord = xcoord.in, ycoord = ycoord.in,
                                   # sizeweight = sizeweight.in, sweight = sweight.in,
                                   stratumID = stratum.in, vartype=vartype,
                                   All_Sites = all_sites)

      }else{

            if(input$cdf_pct=='cdf'){ # Produce CDFs
              estOut <- cont_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in,
                                      vars=input$respVar, weight = input$weightVar,
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      # sizeweight = sizeweight.in, sweight = sweight.in,
                                      stratumID = stratum.in, vartype=vartype,
                                      All_Sites = all_sites,
                                      statistics = 'CDF')$CDF

            }else if(input$cdf_pct=='pct'){ # Just produce percentiles
              estOut <- cont_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in,
                                      vars=input$respVar, weight = input$weightVar,
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      # sizeweight = sizeweight.in, sweight = sweight.in,
                                      stratumID = stratum.in, vartype=vartype,
                                      All_Sites = all_sites,
                                      statistics = c('Pct'))$Pct
            }else if(input$cdf_pct=='mean'){
              estOut <- cont_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in,
                                      vars=input$respVar, weight = input$weightVar,
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      # sizeweight = sizeweight.in, sweight = sweight.in,
                                      stratumID = stratum.in, vartype=vartype,
                                      All_Sites = all_sites,
                                      statistics = c('Mean'))$Mean
            }else{
              estOut <- cont_analysis(dframe = dfIn, siteID=input$siteVar, subpops=subpops.in,
                                      vars=input$respVar, weight = input$weightVar,
                                      xcoord = xcoord.in, ycoord = ycoord.in,
                                      # sizeweight = sizeweight.in, sweight = sweight.in,
                                      stratumID = stratum.in, vartype=vartype,
                                      All_Sites = all_sites,
                                      statistics = c('Total'))$Total
            }
          # }
      }

      remove_modal_spinner()

    if(exists('warn_df') && ncol(warn_df)>1){
      outdf <- list(estOut=estOut, warndf=warn_df)
    }else{
      outdf <- list(estOut=estOut, warndf=data.frame(warnings='none'))
    }


  })


  # Output the population estimates to a table
  output$popest <- DT::renderDataTable({
    dataEst()[['estOut']]
  }, options = list(scrollX=TRUE, scrollY=TRUE, rownames=F, searching=FALSE))

  output$warnest <- DT::renderDataTable({
    dataEst()[['warndf']]
  }, options = list(scrollX=TRUE, scrollY=TRUE, rownames=F, searching=FALSE))


# Download Analysis Results -----------------------------------------------


  # Only enable download button once population estimates are produced
  observe({shinyjs::toggleState('dwnldcsv',length(dataEst()[['estOut']])!=0)})
  # Name output file based on type of analysis selected and write to comma-delimited file
  output$dwnldcsv <- downloadHandler(
    filename = function() {
      if(input$atype=='categ'){
        paste("Categorical_PopEstOutput_",Sys.Date(), ".csv", sep = "")
      }else{
        if(input$cdf_pct=='cdf'){
          paste("Continuous_CDF_Output_",Sys.Date(), ".csv", sep = "")
        }else if(input$cdf_pct=='pct'){
          paste("Continuous_Percentiles_Output_",Sys.Date(), ".csv", sep = "")
        }else if(input$cdf_pct=='mean'){
          paste("Continuous_Means_Output_", Sys.Date(), ".csv", sep="")
        }else{
          paste("Continuous_Totals_Output_", Sys.Date(), ".csv", sep='')
        }
      }
    },
    content = function(file) {
      write.csv(dataEst()[['estOut']], file, row.names = FALSE)
    }
  )

  # Only enable download button once population estimates are produced
  observe({shinyjs::toggleState('chgcsv',length(chgEst()[['chgOut']])!=0)})
  # Name output file based on type of analysis selected and write to comma-delimited file
  output$chgcsv <- downloadHandler(
    filename = function() {
      if(input$chgCatCont == 'chgCat'){
        paste("Change_Categ_Est_Output_",Sys.Date(), ".csv", sep = "")
      }else{
        if(input$testType == 'mean'){
          paste("Change_Contin_Mean_Est_Output_",Sys.Date(), ".csv", sep = "")
        }else{
          paste("Change_Contin_Median_Est_Output_",Sys.Date(), ".csv", sep = "")
        }
      }
    },
    content = function(file) {
      write.csv(chgEst()[['chgOut']], file, row.names = FALSE)
    }
  )

  ###################################### Categorical Plots Server ######################
  userEst <- reactive({
      estOut <- read.csv(req(input$userinput$datapath))
  })

  observeEvent(input$runBtn, {
    updateRadioButtons(session, "catinput", selected = "Current Estimate Data")
  })

  output$catui <- renderUI({
    req(input$catinput != "Current Estimate Data")
    div(id = "userinput1",
        fileInput(
          inputId = "userinput",
          label = strong("Choose Categorical Estimate File"),
          placeholder = "Must be a .csv file",
          accept = c(".csv"))) %>%
      #File input helper
      helper(type = "inline",
             title = "Categorical Estimate File",
             content = paste("Choose a file with the same output which the
                                     spsurvey package", strong("cat_analysis()"),
                             "function renders. If the
                                                 dataset is missing required variables, no
                                                 selections will show up in the dropdown menu.
                                                 The expected and required variables are:", strong("Type,
                                                 Indicator, Subpopulation, Category, NResp, Estimate.P,
                                                 StdError.P, LCB95Pct.P, UCB95Pct.P, Estimate.U,
                                                 StdError.U, LCB95Pct.U, UCB95Pct.U")),
             size = "s", easyClose = TRUE, fade = TRUE)
  })

  plotDataset <- eventReactive(c(input$userinput, input$runBtn, input$catinput), {
    if(input$catinput == "Current Estimate Data" && input$atype == 'categ') {

      dataEst()[['estOut']]

    } else {
      necVars <- c('Type', 'Indicator', 'Subpopulation', 'Category', 'Estimate.P',
                   'StdError.P', 'LCB95Pct.P', 'UCB95Pct.P', 'Estimate.U',
                   'StdError.U', 'LCB95Pct.U', 'UCB95Pct.U')

      validate(need(all(necVars %in% colnames(userEst())),
                    message = "Dataset does not include all variables in standardized output from spsurvey."))

      userEst <- userEst()
      # print(colnames(userEst))
      userEst
    }


  })

  con_choices <- eventReactive(c(input$userinput, input$runBtn, input$catinput),{
    req(plotDataset())
    plotData <- plotDataset()

    plotData <- unique(subset(plotData, !(Category=='Total'), select='Category'))

  })

  # Good Condition Input
  observe({
    cc <- con_choices()

    if(is.null(cc)){
      cc<-""
    } else{
      cc <- subset(cc, !(Category %in% c(input$Fair, input$Poor, input$Not_Assessed, input$Other)))
      dd <- unique(cc$Category)
    }
    updateSelectInput(session, "Good", choices = dd,
                      selected = isolate(input$Good))
  })

  # Fair Condition Input
  observe({
    cc <- con_choices()

    if(is.null(cc)){
      cc<-""
    } else{
      cc <- subset(cc, !(Category %in% c(input$Good, input$Poor, input$Not_Assessed, input$Other)))
      dd <- unique(cc$Category)
    }

    updateSelectInput(session, "Fair", choices = dd,
                      selected = isolate(input$Fair))
  })

  # Poor Condition Input
  observe({
    cc <- con_choices()

    if(is.null(cc)){
      cc<-""
    } else{
      cc <- subset(cc, !(Category %in% c(input$Good, input$Fair, input$Not_Assessed, input$Other)))
      dd <- unique(cc$Category)
    }

    updateSelectInput(session, "Poor", choices = dd,
                      selected = isolate(input$Poor))
  })

  # Not Assessed Condition Input
  observe({
    cc <- con_choices()

    if(is.null(cc)){
      cc<-""
    } else{
      cc <- subset(cc, !(Category %in% c(input$Good, input$Poor, input$Fair, input$Other)))
      dd <- unique(cc$Category)
    }

    updateSelectInput(session, "Not_Assessed", choices = dd,
                      selected = isolate(input$Not_Assessed))
  })

  # Other Condition Input
  observe({
    cc <- con_choices()

    if(is.null(cc)){
      cc<-""
    } else{
      cc <- subset(cc, !(Category %in% c(input$Good, input$Poor, input$Fair, input$Not_Assessed)))
      dd <- unique(cc$Category)
    }

    updateSelectInput(session, "Other", choices = dd,
                      selected = isolate(input$Other))
  })

  observeEvent(input$plotbtn,{

    ind_plot <- plotDataset()

    ind_plot <- unique(ind_plot$Indicator)

    updateSelectInput(session, "Ind_Plot", choices = c(ind_plot))
  })

  observeEvent(input$plotbtn,{
    type_plot <- plotDataset()

    type_plot <- subset(type_plot, !(Type == input$Tot_Pop))

    type_plot.choice <- unique(type_plot$Type)

    updateSelectInput(session, "Type_Plot", choices = c(type_plot.choice))

  })

  #Total Population Label Input
  observeEvent(input$Type_Plot,{

    tot_pop <- plotDataset()
    tot_pop <- subset(tot_pop, Type == input$Type_Plot)
    tot_pop <- unique(tot_pop$Subpopulation)

    updateSelectInput(session, "Tot_Pop", choices = tot_pop)
  })


  observeEvent(input$Ind_Plot,{
    cc_plot <- plotDataset()

    cc_plot <- subset(cc_plot, Indicator == input$Ind_Plot & Category %in% c(input$Good,
                                                input$Fair, input$Poor, input$Not_Assessed, input$Other))

    cc_plot.choice <- unique(cc_plot$Category)

    updateSelectInput(session, "Con_Plot", choices = c(cc_plot.choice))
  })


  Est_plot <- eventReactive(c(input$Tot_Pop, input$Ind_Plot, input$plotbtn, input$indconlim),{
    req(input$Ind_Plot, input$Type_Plot, input$Tot_Pop)

    #Set colors to users Condition classes
    col1 <- rep("#5796d1", length(input$Good))
    col2 <- rep("#EE9A00", length(input$Fair))
    col3 <- rep("#f55b5b", length(input$Poor))
    col4 <- rep("#c47a54", length(input$Not_Assessed))
    col5 <- rep("#d15fee", length(input$Other))
    names(col1) <- input$Good
    names(col2) <- input$Fair
    names(col3) <- input$Poor
    names(col4) <- input$Not_Assessed
    names(col5) <- input$Other
    colors <- c(col1, col2, col3, col4, col5)


    if (input$Estimate == "P Estimates") {
      Dataset <- plotDataset()

      Dataset <- subset(Dataset, select = c('Type', 'Subpopulation', 'Indicator', 'Category',
                                            'Estimate.P', 'StdError.P', 'LCB95Pct.P',
                                            'UCB95Pct.P'))

      Dataset$LCB95Pct.P <- with(Dataset, ifelse(LCB95Pct.P < 0, 0, LCB95Pct.P))
      Dataset$UCB95Pct.P <- with(Dataset, ifelse(UCB95Pct.P > 100, 100, UCB95Pct.P))
      Dataset$Estimate.P <- with(Dataset, round(Estimate.P, 0))
      Dataset$UCB95Pct.P <- with(Dataset, round(UCB95Pct.P, 0))
      Dataset$LCB95Pct.P <- with(Dataset, round(LCB95Pct.P, 0))


      names(Dataset)[names(Dataset) == "Estimate.P"] <- "Estimate"
      names(Dataset)[names(Dataset) == "StdError.P"] <- "StdError"
      names(Dataset)[names(Dataset) == "LCB95Pct.P"] <- "LCB95"
      names(Dataset)[names(Dataset) == "UCB95Pct.P"] <- "UCB95"

    } else {
      Dataset <- plotDataset()

      Dataset <- subset(Dataset, select = c('Type', 'Subpopulation', 'Indicator', 'Category',
                                            'Estimate.U', 'StdError.U', 'LCB95Pct.U',
                                            'UCB95Pct.U'))
      names(Dataset)[names(Dataset) == "Estimate.U"] <- "Estimate"
      names(Dataset)[names(Dataset) == "StdError.U"] <- "StdError"
      names(Dataset)[names(Dataset) == "LCB95Pct.U"] <- "LCB95"
      names(Dataset)[names(Dataset) == "UCB95Pct.U"] <- "UCB95"

     }

    # Fill in for all combinations of Indicator and Category for consistency across
    # subpopulations
    popest <- Dataset

    cats.ind <- unique(Dataset[,c('Indicator', 'Category')])
    cats.sub <- unique(Dataset[,c('Type', 'Subpopulation')])

    cats.comb <- merge(cats.sub, cats.ind)
    popest <- merge(popest, cats.comb, by = c('Type','Subpopulation','Indicator','Category'), all=TRUE)
    popest[is.na(popest)] <- 0

    popest <- subset(Dataset, !(Category == "Total"))

    popest$Category <- with(popest, factor(Category, levels=c(input$Not_Assessed, input$Other,
                                                              input$Poor, input$Fair, input$Good)))

    popest <- subset(popest, Indicator == input$Ind_Plot & Type == input$Type_Plot &
                       Subpopulation == input$Tot_Pop)

    names.popest <- popest$Type
    req(input$Type_Plot %in% names.popest)

    #Create Plots
    P1 <- ggplot(data = popest, aes(x = Category, y = Estimate)) +
      geom_bar(aes(fill = Category, color = Category), alpha = 0.5,
               stat="identity", position = position_dodge()) +
      geom_errorbar(aes(ymin = LCB95, ymax = UCB95, color = Category),
                    size=2, width=0) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      theme_bw() +
      labs(
        title = input$title,
        subtitle= input$Ind_Plot,
        x = NULL)+
      theme(
        plot.title = element_text(size = 16, face = "bold", family="sans", hjust=0.5),
        plot.subtitle = element_text(size = 14, face = "bold", family="sans"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        legend.position="none",
        axis.text.x=element_text(face = "bold", size=14),
        axis.text.y=element_text(face = "bold", size=13),
        axis.title.x = element_text(face = "bold", size=14))


    if (input$Estimate == "P Estimates") {
      P1 <- P1 + geom_text(aes(label=paste(format(Estimate),"%",
                                           sep=""), y=Estimate), hjust = -.05, size = 4,
                           fontface = "bold", color = "#4D4D4D", family="sans",
                           position = position_nudge(x = -0.2)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1),
                           breaks=c(0,25,50,75,100)) +
        coord_flip(ylim=c(0, 110)) +
        labs(y = paste0("Percentage of ", input$resource))
    } else {
      P1 <- P1 + geom_text(aes(label = format(round(Estimate), big.mark = ","), y=Estimate),
                           hjust = -.05, size = 4, fontface = "bold", color = "#4D4D4D",
                           family="sans", position = position_nudge(x = -0.25)) +
        scale_y_continuous(labels = scales::comma) +
        coord_flip() +
        labs(y = input$resource)
    }

    if (input$indconlim == TRUE && input$Estimate == "P Estimates") {
      P1 <- P1 + geom_text(aes(label=paste(format(LCB95),"%",
                                           sep=""), y=LCB95), hjust = 1.1, size = 3.5, fontface = "bold",
                           color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15)) +
        geom_text(aes(label=paste(format(UCB95),"%",
                                  sep=""), y=UCB95), hjust = -.15,size = 3.5, fontface = "bold",
                  color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15))
    }

    if (input$indconlim == TRUE && input$Estimate == "U Estimates") {
      ylim <- max(popest$UCB95) * 1.1

      P1 <- P1 + geom_text(aes(label = format(round(LCB95), big.mark = ","), y=LCB95),
                           hjust = 1.1, size = 3.5, fontface = "bold", color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15)) +
        geom_text(aes(label = format(round(UCB95), big.mark = ","), y=UCB95),
                  hjust = .2, size = 3.5, fontface = "bold", color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15)) +
        ylim(0, ylim)
    }

    print(P1)


  })

  SubEst_plot <- eventReactive(c(input$plotbtn, input$goodsort, input$subconlim,
                                 input$Type_Plot, input$Ind_Plot, input$Con_Plot),{
    req(input$Type_Plot %nin% c("ALL SITES", "All Sites", "all sites", "All_Sites", "All.Sites", "all_sites", "all.sites", "National", "national", "NATIONAL", "Statewide", "statewide", "STATEWIDE"))


    #Set colors to users Condition classes
    col1 <- rep("#5796d1", length(input$Good))
    col2 <- rep("#EE9A00", length(input$Fair))
    col3 <- rep("#f55b5b", length(input$Poor))
    col4 <- rep("#c47a54", length(input$Not_Assessed))
    col5 <- rep("#d15fee", length(input$Other))
    names(col1) <- input$Good
    names(col2) <- input$Fair
    names(col3) <- input$Poor
    names(col4) <- input$Not_Assessed
    names(col5) <- input$Other
    colors <- c(col1, col2, col3, col4, col5)


    if (input$Estimate == "P Estimates") {
      Dataset <- plotDataset()

      Dataset <- subset(Dataset, select = c('Type', 'Subpopulation', 'Indicator', 'Category',
                                            'Estimate.P', 'StdError.P', 'LCB95Pct.P',
                                            'UCB95Pct.P'))
      Dataset$LCB95Pct.P <- with(Dataset, ifelse(LCB95Pct.P < 0, 0, LCB95Pct.P))
      Dataset$UCB95Pct.P <- with(Dataset, ifelse(UCB95Pct.P > 100, 100, UCB95Pct.P))
      Dataset$Estimate.P <- with(Dataset, round(Estimate.P, 0))
      Dataset$UCB95Pct.P <- with(Dataset, round(UCB95Pct.P, 0))
      Dataset$LCB95Pct.P <- with(Dataset, round(LCB95Pct.P, 0))


      names(Dataset)[names(Dataset) == "Estimate.P"] <- "Estimate"
      names(Dataset)[names(Dataset) == "StdError.P"] <- "StdError"
      names(Dataset)[names(Dataset) == "LCB95Pct.P"] <- "LCB95"
      names(Dataset)[names(Dataset) == "UCB95Pct.P"] <- "UCB95"

    } else {
      Dataset <- plotDataset()

      Dataset <- subset(Dataset, select = c('Type', 'Subpopulation', 'Indicator', 'Category',
                                            'Estimate.U', 'StdError.U', 'LCB95Pct.U',
                                            'UCB95Pct.U'))
      names(Dataset)[names(Dataset) == "Estimate.U"] <- "Estimate"
      names(Dataset)[names(Dataset) == "StdError.U"] <- "StdError"
      names(Dataset)[names(Dataset) == "LCB95Pct.U"] <- "LCB95"
      names(Dataset)[names(Dataset) == "UCB95Pct.U"] <- "UCB95"

    }

    popest2 <- Dataset

    popest2 <- subset(popest2, Type == input$Type_Plot)


    cats.ind.1 <- unique(popest2[,c('Indicator', 'Category')])
    cats.sub.1 <- unique(popest2[,c('Type', 'Subpopulation')])

    cats.comb.1 <- merge(cats.sub.1, cats.ind.1)
    popest2 <- merge(popest2, cats.comb.1, by = c('Type','Subpopulation','Indicator','Category'),
                     all=TRUE)
    # popest2 <- tidyr::complete(popest2, Type, Subpopulation, nesting(Indicator, Category))

    popest2[is.na(popest2)] <- 0

    popest2 <- subset(popest2, Indicator == input$Ind_Plot & !(Category == 'Total'))
    # popest2$Subpopulation <-factor(popest2$Subpopulation, levels = rev(unique(popest2$Subpopulation)))

    # Creates vector of category variables
    firstcon <- popest2
    # NOT SURE WHAT THIS IS TRYING TO DO AND WHY SO COMPLEX
    firstcon$Category <- factor(firstcon$Category, levels = c(input$Good, input$Fair,
                                                              input$Poor, input$Other,
                                                              input$Not_Assessed))

    firstcon <- firstcon[order(firstcon$Type, firstcon$Subpopulation, firstcon$Category),]

    # firstcon <- dplyr::arrange(firstcon, Type, Subpopulation,
    #                            factor(Category, levels=c(.env$input$Good, .env$input$Fair, .env$input$Poor, .env$input$Other, .env$input$Not_Assessed)))
    # firstcon$Type <- with(firstcon, factor(Category, levels=c(input$Good, input$Fair, input$Poor, input$Other, input$Not_Assessed)))
    firstcon <- subset(firstcon, Category %in% c(input$Good, input$Fair, input$Poor, input$Other, input$Not_Assessed))
    firstcon <- unique(firstcon$Category)

    # Creates vector of subpopulations by order of users 'good' condition
    suborder <- popest2
    suborder <- subset(suborder, Category == firstcon[1])
    suborder <- suborder[order(suborder$Estimate),]

    #suborder$Subpopulation <- forcats::fct_reorder(suborder$Subpopulation, suborder$Estimate)
    suborder <- unique(suborder$Subpopulation)

    if(input$goodsort == TRUE){
      # Filters and Arranges user data based on good condition
      popest2 <- subset(popest2, Category == input$Con_Plot)
      popest2$Subpopulation <- factor(popest2$Subpopulation, levels = suborder)
    } else {
      popest2 <- subset(popest2, Category == input$Con_Plot)
      popest2$Subpopulation <- factor(popest2$Subpopulation,
                                      levels = rev(unique(popest2$Subpopulation)))
    }

    names <- popest2$Type
    # pluck(popest2, "Type")
    req(input$Type_Plot %in% names)

    P2 <- ggplot(data = popest2, aes(x = Subpopulation, y = Estimate)) +
      geom_bar(aes(fill = Category, color = Category), alpha = 0.5, stat="identity",
               position = position_dodge()) +
      geom_errorbar(aes(ymin = LCB95, ymax = UCB95, color = Category), size=2, width=0) +
      scale_fill_manual(values = colors) +
      scale_color_manual(values = colors) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
      theme_bw() +
      labs(
        title = input$title,
        subtitle= input$Ind_Plot,
        x = NULL)+
      theme(
        plot.title = element_text(size = 16, face = "bold", family="sans", hjust=0.5),
        plot.subtitle = element_text(size = 15, face = "bold", family="sans"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        legend.position="none",
        axis.text.x=element_text(face = "bold", size=14),
        axis.text.y=element_text(face = "bold", size=13),
        axis.title.x = element_text(face = "bold", size=14))

    if (input$Estimate == "P Estimates") {
      P2 <- P2 + geom_text(aes(label=paste(format(Estimate),"%",
                                           sep=""), y=Estimate), hjust = -.05, size = 4,
                           fontface = "bold", color = "#4D4D4D", family="sans",
                           position = position_nudge(x = -0.2)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1), breaks=c(0,25,50,75,100)) +
        coord_flip(ylim=c(0, 110)) +
        labs(y = paste0("Percentage of ", input$resource))
    } else {
      P2 <- P2 + geom_text(aes(label = format(round(Estimate), big.mark = ","), y=Estimate),
                           hjust = -.05, size = 4, fontface = "bold", color = "#4D4D4D",
                           family="sans", position = position_nudge(x = -0.2)) +
        scale_y_continuous(labels = scales::comma) +
        coord_flip() +
        labs(y = paste0("Amount of ", input$resource))
    }

    if (input$subconlim == TRUE && input$Estimate == "P Estimates") {
      P2 <- P2 + geom_text(aes(label=paste(format(LCB95),"%",
                                           sep=""), y=LCB95), hjust = 1.1, size = 3.5,
                           fontface = "bold",
                           color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15)) +
        geom_text(aes(label=paste(format(UCB95),"%",
                                  sep=""), y=UCB95), hjust = -.15, size = 3.5, fontface = "bold",
                  color = "#4D4D4D", family="sans", position = position_nudge(x = 0.15))
    }

    if (input$subconlim == TRUE && input$Estimate == "U Estimates") {
      ylim <- max(popest2$UCB95) * 1.1

      P2 <- P2 + geom_text(aes(label = format(round(LCB95), big.mark = ","), y=LCB95),
                           hjust = 1.1, size = 3.5, fontface = "bold", color = "#4D4D4D",
                           family="sans", position = position_nudge(x = 0.15)) +
        geom_text(aes(label = format(round(UCB95), big.mark = ","), y=UCB95),
                  hjust = .2, size = 3.5, fontface = "bold", color = "#4D4D4D", family="sans",
                  position = position_nudge(x = 0.15)) +
        ylim(0, ylim)
    }

    print(P2)
  })

  output$plot <- renderPlot({
    Est_plot()

  })

  calcheight <- reactive({
    popest <- plotDataset()
    # Prevents plotting error
    names.popest <- popest$Type

    req(input$Type_Plot %in% names.popest)

    # popest <- plotDataset()
    popest <- unique(subset(popest, Type == input$Type_Plot, select = 'Subpopulation'))

    if(length(popest$Subpopulation) < 4){
      calcheight <- 120 * length(popest$Subpopulation)
    }else{
      calcheight <- 70 * length(popest$Subpopulation)
    }
  })

  output$plot2 <- renderPlot({
    req(calcheight())
    calcheight <- calcheight()
    SubEst_plot()
  }, height = calcheight)


  output$downloadPlot1 <- downloadHandler(
    filename = function() { paste("Indicator_Estimates-", Sys.Date(), '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = Est_plot(), width=8, height=4)
    }
  )

  output$downloadPlot2 <- downloadHandler(
    filename = function() { paste("SubPop_Estimates-", Sys.Date(), '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = SubEst_plot())
    }
  )
  ####################### Continuous Server ##############################
  userCDFEst <- reactive({
    ConEstOut <- read.csv(req(input$ConCDFinput$datapath))
  })


  observeEvent(input$runBtn, {
    updateRadioButtons(session, "coninput", selected = "Current Estimate Data")
  })

  output$conui <- renderUI({
    req(input$coninput != "Current Estimate Data")
    fileInput(
      inputId = "ConCDFinput",
      label = strong("Choose CDF Analysis file"),
      placeholder = "Must be a .csv file",
      accept = c(".csv")) %>%
      #File input helper
      helper(type = "inline",
             title = "CDF Estimate File",
             content = paste("Choose a file with the same output which the
                                     spsurvey package", strong("cont_analysis()"),
                             "function renders. If the
                                                 dataset is missing required variables, no
                                                 selections will show up in the dropdown menu.
                                     The expected and required variables are:", strong("Type,
                                     Subpopulation, Indicator, Value, Estimate.P, Estimate.U,
                                     StdError.P, StdError.U, LCB95Pct.P, UCB95Pct.P, LCB95Pct.U,
                                     UCB95Pct.U")),
             size = "s", easyClose = TRUE, fade = TRUE)
  })


  CDFDataset <- eventReactive(c(input$ConCDFinput, input$runBtn, input$coninput), {

    if(input$coninput == "Current Estimate Data" && input$cdf_pct=='cdf') {
      dataEst()[['estOut']]
    } else {
      CDFOut <- userCDFEst()

    }})



  observe({
    indcon_choice <- CDFDataset()

    indcon_choice <- unique(indcon_choice$Indicator)

    updateSelectInput(session, "Ind_Con", choices = indcon_choice)
  })

  observe({
    popcon_choice <- CDFDataset()

    popcon_choice <- unique(popcon_choice$Type)

    updateSelectInput(session, "Pop_Con", choices = popcon_choice)
  })

  observe({
    req(input$Pop_Con)
    subpopcon_choice <- CDFDataset()

    subpopcon_choice <- subset(subpopcon_choice, Type==input$Pop_Con, select = Subpopulation)

    subpopcon_choice <- unique(subpopcon_choice$Subpopulation)

    allsites_sub <- CDFDataset()

    allsites_sub <- subset(allsites_sub, Type %in% c("ALL SITES", "All Sites", "all sites", "All_Sites", "All.Sites", "all_sites", "all.sites", "National", "national", "NATIONAL", "Statewide", "statewide", "STATEWIDE"), select = 'Subpopulation')

    allsites_sub.choice <- unique(allsites_sub$Subpopulation)

    updateSelectInput(session, "SubPop_Con", choices = c(allsites_sub.choice, subpopcon_choice),
                      selected = allsites_sub.choice[1])
  })

  CDF_subplot <- reactive({
    req(input$plotbtncon, input$SubPop_Con)
    necVars <- c('Type', 'Subpopulation', 'Indicator', 'Value',
                 'Estimate.P', 'Estimate.U',
                 'StdError.P', 'StdError.U', 'LCB95Pct.P',
                 'UCB95Pct.P', 'LCB95Pct.U',
                 'UCB95Pct.U')

    validate(need(all(necVars %in% colnames(CDFDataset())),
                  message = "Dataset does not include all variables in standardized output from spsurvey."))

    CDFDataset <- CDFDataset()

    if (input$Estimate_CDF == "P Estimates_CDF") {
      names(CDFDataset)[names(CDFDataset) == "Estimate.P"] <- "Estimate"
      names(CDFDataset)[names(CDFDataset) == "StdError.P"] <- "StdError"
      names(CDFDataset)[names(CDFDataset) == "LCB95Pct.P"] <- "LCB95"
      names(CDFDataset)[names(CDFDataset) == "UCB95Pct.P"] <- "UCB95"
      CDFDataset <- subset(CDFDataset, select = c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate', 'StdError', 'LCB95', 'UCB95'))
    } else {
      names(CDFDataset)[names(CDFDataset) == "Estimate.U"] <- "Estimate"
      names(CDFDataset)[names(CDFDataset) == "StdError.U"] <- "StdError"
      names(CDFDataset)[names(CDFDataset) == "LCB95Pct.U"] <- "LCB95"
      names(CDFDataset)[names(CDFDataset) == "UCB95Pct.U"] <- "UCB95"
      CDFDataset <- subset(CDFDataset, select = c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate', 'StdError', 'LCB95', 'UCB95'))
    }

    CDFDataset <- subset(CDFDataset, Indicator == input$Ind_Con & Subpopulation %in% input$SubPop_Con)
    CDFDataset$Subpopulation <-factor(CDFDataset$Subpopulation, levels = rev(unique(CDFDataset$Subpopulation)))

    g <- ggplot(CDFDataset, aes(y=Estimate, x=Value, color = Subpopulation, fill = Subpopulation)) +
      geom_step(size=1) +
      scale_colour_viridis_d("Population", guide = guide_legend(reverse = TRUE)) +
      scale_fill_viridis_d() +
      theme_bw() +
      labs(
        title = input$title2,
        subtitle= "",
        x = paste0(input$Ind_Con," ",input$units)
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", family="sans", hjust=0.5),
        plot.subtitle = element_text(size = 14, face = "bold", family="sans"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        legend.title = element_text(face = "bold", size=14),
        legend.text = element_text(face = "bold", size=12),
        axis.text.x=element_text(face = "bold", size=14),
        axis.text.y=element_text(face = "bold", size=13),
        axis.title.x = element_text(face = "bold", size=14),
        axis.title.y = element_text(face = "bold", size=14))


    if(input$Estimate_CDF == "P Estimates_CDF") {
      g <- g + scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        labs(y = paste0("Cumulative Probability ", input$resource2))
    } else {
      g <- g + scale_y_continuous(labels = scales::comma) +
        labs(y = paste0("Cumulative amount of ", input$resource2))
    }

    if (is.numeric(input$Thresh)) {
      g <- g + geom_vline(xintercept = input$Thresh, color = "red", size = 1, linetype = "longdash")
    }
    if (input$conflim == TRUE) {
      g <- g + geom_ribbon(aes(ymin = LCB95, ymax = UCB95, fill = Subpopulation), alpha = 0.2,
                           colour = "transparent", show.legend = FALSE)
    }
    if (input$log == TRUE) {
      g <-  g + scale_x_continuous(trans='log10')
    }

    g
  })

  Dist_plot <- reactive({
    req(input$plotbtncon)
    CDFDataset <- CDFDataset()
    if (input$Estimate_CDF == "P Estimates_CDF") {

      names(CDFDataset)[names(CDFDataset) == "Estimate.P"] <- "Estimate"
      names(CDFDataset)[names(CDFDataset) == "StdError.P"] <- "StdError"
      names(CDFDataset)[names(CDFDataset) == "LCB95Pct.P"] <- "LCB95"
      names(CDFDataset)[names(CDFDataset) == "UCB95Pct.P"] <- "UCB95"
      CDFDataset <- subset(CDFDataset, select = c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate', 'StdError', 'LCB95', 'UCB95'))
    } else {

      names(CDFDataset)[names(CDFDataset) == "Estimate.U"] <- "Estimate"
      names(CDFDataset)[names(CDFDataset) == "StdError.U"] <- "StdError"
      names(CDFDataset)[names(CDFDataset) == "LCB95Pct.U"] <- "LCB95"
      names(CDFDataset)[names(CDFDataset) == "UCB95Pct.U"] <- "UCB95"
      CDFDataset <- subset(CDFDataset, select = c('Type', 'Subpopulation', 'Indicator', 'Value', 'Estimate', 'StdError', 'LCB95', 'UCB95'))
    }

    CDFDataset <- subset(CDFDataset, Indicator == input$Ind_Con & Subpopulation %in% input$SubPop_Con)
    #CDFDataset <- with(CDFDataset, reorder(Subpopulation, Estimate, decreasing=FALSE))
    CDFDataset$Subpopulation <-factor(CDFDataset$Subpopulation, levels = rev(unique(CDFDataset$Subpopulation)))
    #CDFDataset <- suborder[order(CDFDataset$Estimate),]
    # CDFDataset$Subpopulation <- forcats::fct_reorder(CDFDataset$Subpopulation, CDFDataset$Estimate)

    g <- ggplot(CDFDataset, aes(x = Estimate, y = Subpopulation, fill=Subpopulation)) +
      scale_fill_viridis_d("Population") +
      labs(
        title = input$title2,
        subtitle= "",
        y = "Frequency") +
      theme(
        plot.title = element_text(size = 16, face = "bold", family="sans", hjust=0.5),
        plot.subtitle = element_text(size = 14, face = "bold", family="sans"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        legend.title = element_text(face = "bold", size=14),
        legend.text = element_text(face = "bold", size=12),
        axis.text.x=element_text(face = "bold", size=14),
        axis.text.y=element_text(face = "bold", size=13),
        axis.title.x = element_text(face = "bold", size=14),
        axis.title.y = element_text(face = "bold", size=14),
        legend.position = "none")

    if(input$Estimate_CDF == "P Estimates_CDF") {
      g <- g + scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
        labs(x = paste0("Percentage of ", input$resource2))

    } else {
      g <- g + scale_x_continuous(labels = scales::comma) +
        labs(x = paste0("Amount of ", input$resource2))
    }

    if (length(input$SubPop_Con) == 1 && input$Estimate_CDF == "U Estimates_CDF") {
      g + geom_density_ridges(scale = 100000, size = 0.9, jittered_points = TRUE,
                              position = position_points_jitter(width = 0.5, height = 0),
                              point_shape = "|", point_size = 3,
                              quantile_lines = TRUE, alpha=0.8)
    } else if (length(input$SubPop_Con) == 1)  {
      g + geom_density_ridges(scale = 50, size = 0.5, jittered_points = TRUE,
                              position = position_points_jitter(width = 0.5, height = 0),
                              point_shape = "|", point_size = 3,
                              quantile_lines = TRUE, alpha=0.8)
    } else {
      g + geom_density_ridges(size = 1, rel_min_height = 0.03, jittered_points = TRUE,
                              position = position_points_jitter(width = 0.5, height = 0),
                              point_shape = "|", point_size = 3,
                              quantile_lines = TRUE, alpha=0.8)
    }
  })

  output$CDFsubplot <- renderPlot({
    req(input$plotbtncon)
    CDF_subplot()
  })


  output$download1 <- downloadHandler(
    filename = function() {paste("CDF_Plot-", Sys.Date(), ".png", sep="")},
    content = function(file) {
      ggsave(file, CDF_subplot())
    })

  calcheight2 <- reactive({
    req(input$SubPop_Con)
    popcount <- CDFDataset()

    popcount <- unique(subset(popcount, Subpopulation %in% input$SubPop_Con, select = 'Subpopulation'))

    if (length(input$SubPop_Con) < 3) {
      calcheight <- 250
    } else {
      calcheight <- 100 * length(popcount$Subpopulation)}})


  output$Distplot <- renderPlot({
    req(calcheight2())
    calcheight <- calcheight2()
    Dist_plot()
  }, height = calcheight2)

  output$download2 <- downloadHandler(
    filename = function() {paste("CDF_Distribution-", Sys.Date(), ".png", sep="")},
    content = function(file) {
      ggsave(file, Dist_plot())
    })


  # End session if browser window is closed
  if(!dir.exists('/home/vcap/app')) {
    session$onSessionEnded(function() {
        # rm(warn_df,envir=.GlobalEnv)
       
      stopApp()
    })
  }

}

# Run the application
shinyApp(ui = ui, server = server)
