\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{lawdoc}[2015/08/04 Texas Law Docs]

\LoadClass[14pt]{extarticle}


\RequirePackage[T1]{fontenc}
\RequirePackage[pdftex]{graphicx}
\RequirePackage{newcent}
\RequirePackage{etoolbox}
\RequirePackage{setspace}
\RequirePackage[letterpaper,top=1in,left=1.5in,right=1.5in,bottom=1in]{geometry}
\RequirePackage{enumitem}
\setlist[itemize,1]{label=---}
\setlist[itemize,2]{label=$\circ$}



\setcounter{secnumdepth}{5}
\setcounter{tocdepth}{5}

\setlength\parindent{0pt}
\setlength\parskip{3mm}


%Syntax for @startsection{name}{level}{indent}{abovespace}{belowspace}{format}
%The 4th argument is negative to suppress indenting after the section heading
\def\section{\@startsection{section}{2}{0pt}{-10pt}{10pt}{\normalsize\bfseries}}%
\def\subsection{\@startsection{subsection}{2}{0.5in}{-2pt}{2pt}{\hyphenpenalty=10000\normalsize\bfseries}}%
\def\subsubsection{\@startsection{subsubsection}{3}{0pt}{-2pt}{2pt}{\hyphenpenalty=10000\bfseries}}%
\def\paragraph{\@startsection{paragraph}{4}{0pt}{-2pt}{2pt}{\hyphenpenalty=10000\bfseries}}%
\def\subparagraph{\@startsection{subparagraph}{5}{0pt}{-2pt}{2pt}{\hyphenpenalty=10000\bfseries}}%

\def\presection{}
\def\presubsection{}
\def\presubsubsection{}
\def\preparagraph{}
\def\presubparagraph{}
\def\postsection{.}
\def\postsubsection{}
\def\postsubsubsection{}
\def\postparagraph{}
\def\postsubparagraph{}

\def\issue{\subsection{}}
\def\@seccntformat#1{\csname pre#1\endcsname\csname the#1\endcsname\csname post#1\endcsname\quad}
\def\numberline@tmp#1{\hb@xt@\@tempdima{#1\hfil}}

\def\@sect#1#2#3#4#5#6[#7]#8{%
  \ifnum #2>\c@secnumdepth
    \let\@svsec\@empty
  \else
    \refstepcounter{#1}%
    \protected@edef\@svsec{\@seccntformat{#1}\relax}%
  \fi
  \@tempskipa #5\relax
  \ifdim \@tempskipa>\z@
    \begingroup
      #6{%
        \@hangfrom{\hskip #3\relax\@svsec}%
          \interlinepenalty \@M #8\@@par}%
    \endgroup
    \csname #1mark\endcsname{#7}%
    \addcontentsline{toc}{#1}{%
      \ifnum #2>\c@secnumdepth \else
        \protect\numberline@tmp{\csname pre#1\endcsname\csname the#1\endcsname}%
      \fi
      #8}%
  \else
    \def\@svsechd{%
      #6{\hskip #3\relax
      \@svsec #8}%
      \csname #1mark\endcsname{#7}%
      \addcontentsline{toc}{#1}{%
        \ifnum #2>\c@secnumdepth \else
          \protect\numberline{\csname pre#1\endcsname\csname the#1\endcsname}%
        \fi
        #8}}%
  \fi
  \@xsect{#5}}


\renewcommand{\thesection}{\arabic{section}}
\renewcommand{\thesubsection}{\arabic{subsection}}%\arabic{subsection}
\renewcommand\thesubsubsection{\thesubsection.\@arabic\c@subsubsection}
\renewcommand\theparagraph    {\thesubsubsection.\@arabic\c@paragraph}
\renewcommand\thesubparagraph {\theparagraph.\@arabic\c@subparagraph}


\newcommand\CauseNo[1]{\def\@CauseNo{#1}}
\global\let\@CauseNo\@empty
%\newcommand\Petitioner[1]{\def\@petitioner{#1}}
\newcommand{\Court}[1]{\def\@Court{#1}}

\newcommand\CounselforPetitioner[1]{\def\@CounselforPetitioner{#1}}
\newcommand\CounselforPetitionerAddress[1]{\def\@CounselforPetitionerAddress{#1}}





\newcommand\CounselforPetitionerTitle{Counsel for Petitioner}


%\newcommand\printinfo[1]{\parbox[t]{0.5\textwidth}{{\itshape\csname#1Title\endcsname}}\parbox[t]{0.5\textwidth}{\csname @#1\endcsname}}





\newcommand{\sectionline}{\vbox{\baselineskip=1.5pt \hbox{\rule{\linewidth}{.7pt}}\hbox{\rule{\linewidth}{.7pt}}}}
\newcommand{\smallrule}{\rule{.3\linewidth}{.7pt}}


%\newcommand{\address}[1]{\def\@address{#1}}

\def\@firstparty{\@latex@warning@no@line{No \noexpand\firstparty given}}


\renewcommand{\maketitle}{\make@title}

\newcommand{\make@title}{
{\begin{center}
No. \ifx\@CauseNo\@empty\rule{0.25\textwidth}{0.7pt}\else\@CauseNo\fi\\[3mm]
\parbox[c]{0.4\textwidth}{\begin{center}\begin{spacing}{1.2}State of Texas\\v.\\John Doe\end{spacing}\end{center}}
\parbox[c]{1cm}{\rule{0.7pt}{3cm}}
\parbox[c]{0.4\textwidth}{\begin{center}\begin{spacing}{1.2}\@Court\end{spacing}\end{center}}\par
\vskip 1cm
{\bfseries\@title}
\end{center}
}}%\vspace*{3mm}


\newcommand\makesignature{%\singlespacing
%\label{signatureblock}%\hspace{0.5\textwidth}
\noindent\parbox[t]{0.5\textwidth}{%
/s/\\[-15pt]
\rule{5cm}{.7pt} \\
\@CounselforPetitioner\\
\@CounselforPetitionerAddress\\
{\itshape\CounselforPetitionerTitle}
}
}

\newcommand\makesignatureline[1]{\parbox[t]{5cm}{\rule{5cm}{.7pt}\\
#1}
}

\clubpenalty=3000
\widowpenalty=3000

\pagestyle{empty}

\endinput
