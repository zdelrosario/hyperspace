%%
% NATO RTO class for Meeting Proceedings papers and Educational Notes
%   by Bil Kleb
%
% TODO:
% - Annexes and appendices
% - a4paper option
% - Add hook for draft footer note (to mark draft, version, etc.)

\NeedsTeXFormat{LaTeX2e}[1994/12/01]
\ProvidesClass{nato-sto}[2007/09/19 v1.3.3 NATO RTO MP papers or ENs (unofficial)]

% class options
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}%
                \typeout{Unknown option: passing ``\CurrentOption"
                         option on to the underlying LaTeX article class.}}
\ProcessOptions

% use article class as base
\LoadClass[letterpaper,twoside,11pt]{article}

%%
% page layout
\RequirePackage[letterpaper,
                hmargin=0.90in,
                tmargin=1.25in,
                headheight=0.75in,
                headsep=0.25in,
                footskip=2\baselineskip,
                bmargin=1.25in]{geometry}

%%
% accommodate images
\RequirePackage{graphicx}

% default behavior
\setkeys{Gin}{ width=\linewidth, totalheight=\textheight, keepaspectratio }

% image search path
\graphicspath{{graphics/}}

%%
% page header/footer
\RequirePackage{fancyhdr}

% paper number, publication reference, and classification commands
% parallel to \title/\@title definitions from base/latex.ltx
\def\papernumber#1{\gdef\@nato@sto@paper@number{#1}}
\def\@nato@sto@paper@number{%
  \@latex@error{No \noexpand\papernumber supplied}\@ehc}

\def\publicationreference#1{\gdef\@nato@sto@publication@reference{#1}}
\def\@nato@sto@publication@reference{%
  \@latex@error{No \noexpand\publicationreference supplied}\@ehc}

\def\classification#1{\gdef\@nato@sto@classification{#1}}
\def\@nato@sto@classification{%
  \@latex@warning@no@line{No \noexpand\classification given,
                          leaving empty => UNCLASSIFID/UNLIMITED}
  \gdef\@nato@sto@classification{}}

% header/footer rules
\renewcommand{\headrulewidth}{1pt}
\renewcommand{\footrulewidth}{1pt}

% plain page style used by \maketitle (first page)
\fancypagestyle{plain}{%
  \fancyhf{} % clear all fields
  \lhead{\includegraphics[width=0.91in]{nato-otan_logo}}
  \rhead{\includegraphics[width=0.91in]{nato-sto_logo}}
  \lfoot{\bfseries\sffamily\footnotesize \@nato@sto@publication@reference}
  \rfoot{\bfseries\sffamily\footnotesize \@nato@sto@paper@number - \thepage}
  \chead{\raisebox{2.5\baselineskip}%
                  {\bfseries\sffamily\footnotesize
                   \@nato@sto@classification}}
  \cfoot{\raisebox{-2\baselineskip}%
                  {\bfseries\sffamily\footnotesize
                   \@nato@sto@classification}}
}

% nominal page style
\pagestyle{fancy}
\fancyhf{} % clear all fields
\fancyhead[RO,LE]{\bfseries\sffamily \@title}
\fancyhead[LO]{\includegraphics[width=0.91in]{nato-otan_logo}}
\fancyhead[RE]{\includegraphics[width=0.91in]{nato-sto_logo}}
\fancyfoot[LO,RE]{\bfseries\sffamily\footnotesize
                  \@nato@sto@publication@reference}
\fancyfoot[RO,LE]{\bfseries\sffamily\footnotesize
                  \@nato@sto@paper@number - \thepage}
\fancyhead[C]{\raisebox{2.5\baselineskip}%
                       {\bfseries\sffamily\footnotesize
                        \@nato@sto@classification}}
\fancyfoot[C]{\raisebox{-2\baselineskip}%
                       {\bfseries\sffamily\footnotesize
                        \@nato@sto@classification}}

%%
% Use required font
\RequirePackage{times}

%%
% Colors
\RequirePackage{color}

\definecolor{darkblue}{rgb}{0.0,0.0,0.5}

%%
% Enable hyperlinks
\RequirePackage[hyperfootnotes=false,
                hyperfigures,
                breaklinks,
                colorlinks,
                urlcolor=darkblue,
                filecolor=darkblue,
                citecolor=black,
                linkcolor=black,
                pagecolor=black,
                menucolor=black,
                anchorcolor=black]{hyperref}

%%
% Citations
\RequirePackage[numbers,sort&compress]{natbib}
\RequirePackage{hypernat}

% To make references section name upper case -- original from latex.ltx
\renewenvironment{thebibliography}[1]
     {\section*{\MakeUppercase\refname}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

%%
% Titling -- modified version of latex/base/article.cls

\renewcommand\maketitle{
  \begingroup
    \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
    \def\@makefnmark{\rlap{\@textsuperscript{\normalfont\@thefnmark}}}%
    \long\def\@makefntext##1{\parindent 1em\noindent
            \hb@xt@1.8em{%
                \hss\@textsuperscript{\normalfont\@thefnmark}}##1}%
    \newpage
    \global\@topnum\z@
    \@maketitle
    \thispagestyle{plain}\@thanks
    \enlargethispage{-2cm}% reserve space NATO-sto publication footnote
  \endgroup
  \setcounter{footnote}{0}%
  \global\let\thanks\relax
  \global\let\maketitle\relax
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@author\@empty
  \global\let\@date\@empty
  \global\let\title\relax
  \global\let\author\relax
  \global\let\date\relax
  \global\let\and\relax
}
\def\@maketitle{%
  \begin{center}%
    \let \footnote \thanks
    {\LARGE\bfseries \textcolor[rgb]{0.5,0,0}{\@title} \par}%
    \vskip 1em%
    {\normalsize
      \lineskip .5em%
      \begin{tabular}[t]{c}%
        \@author
      \end{tabular}\par}%
  \end{center}%
}

%%
% Abstract

\renewenvironment{abstract}%
  {\section*{\itshape\MakeUppercase\abstractname}\itshape}%
  {\vskip 1em}

%%
% Section headers -- modifying those from base/article.cls

\setcounter{secnumdepth}{4}

\renewcommand\thesection      {\@arabic\c@section.\@arabic\c@subsection}
\renewcommand\thesubsection   {\thesection}

\renewcommand\section{\@startsection {section}{1}{\z@}%
                                     {-3.5ex \@plus -1ex \@minus -.2ex}%
                                     {2.3ex \@plus.2ex}%
                                     {\normalfont\large\bfseries\uppercase}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                       {-3.25ex\@plus -1ex \@minus -.2ex}%
                                       {1.5ex \@plus .2ex}%
                                       {\normalfont\large\bfseries}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                       {-3.25ex\@plus -1ex \@minus -.2ex}%
                                       {1.5ex \@plus .2ex}%
                                       {\normalfont\normalsize\bfseries}}
\renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                      {-3.25ex \@plus -1ex \@minus -.2ex}%
                                      {1ex \@plus .2ex}%
                                      {\normalfont\normalsize\itshape}}

%%
% Caption font -- original from base/article.cls

\long\def\@makecaption#1#2{%
  \vskip\abovecaptionskip
  \sbox\@tempboxa{\scriptsize\bfseries\sffamily #1: #2}%
  \ifdim \wd\@tempboxa >\hsize
    \scriptsize\bfseries\sffamily
    #1: #2\par
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \fi
  \vskip\belowcaptionskip}

%%
% Insert NATO logo page if document has an odd number of pages --
% adapted from Piet van Oostrum's fancyhdr documentation

\AtEndDocument{
  \clearpage
  \ifodd\c@page
    \relax
  \else
    \hbox{}
    \vspace*{\fill}
    \begin{center}
      \includegraphics[width=0.5in]{nato_logo}
    \end{center}
    \vspace{\fill}
    \newpage
  \fi
}
