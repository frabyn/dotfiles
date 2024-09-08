%===============================================================================
% court-paper.cls - Class file for a Texas appellate brief, derived from lawbrief.cls
% Copyright (C) 2015 Franklin Bynum
%
% This file derived from the LaWTeX package, for more information, see:
%   Project Homepage: http://www.cs.princeton.edu/~cdecoro/lawtex/
%   Code Repository: https://sourceforge.net/projects/lawtex/
%
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%===============================================================================

%%\section{Law Brief Document Class}
%%The file texas-appellate-brief.cls provides a flexible document class for appellate-style legal briefs,
%%including cover page, table of contents, tables of authorities, etc.

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{court-paper}

\newif\if@trial
\@trialfalse
\DeclareOption{trial}{\@trialtrue}


\newif\if@appellate
\@appellatefalse
\DeclareOption{appellate}{\@appellatetrue}

\newif\if@expunction
\@expunctionfalse
\DeclareOption{expunction}{\@expunctiontrue}



\DeclareOption*{\PassOptionsToClass{\CurrentOption}{extarticle}}
\ProcessOptions\relax
\LoadClass{extarticle}



\PassOptionsToClass{casesupra}{bluebook}
\RequirePackage[T1]{fontenc}
\RequirePackage[pdftex]{graphicx}
%\RequirePackage{newcent}
\RequirePackage{array}
\RequirePackage{bluebook}
\RequirePackage{lawdata}
\RequirePackage{etoolbox,calc,kvoptions,ifthen}%keyvals
\RequirePackage{indentfirst}
\RequirePackage{setspace}
\RequirePackage{suffix}
\RequirePackage{pdftexcmds}

\input texas-courts.tex
\input places-data.tex
\input persons-data.tex

\newcommand{\@RunMakeIndex}{%
	\clearpage%
	\immediate\closeout\Case@idxfile%
	\immediate\closeout\Statute@idxfile%
	\immediate\closeout\Other@idxfile%
	\immediate\write18{makeindex -r -s lawcitations.ist Case}%
	\immediate\write18{makeindex -L -r -s lawcitations.ist Statute}%
	\immediate\write18{makeindex -r -s lawcitations.ist Other}%
}

\AtEndDocument{%
	\@RunMakeIndex%
}

\setcounter{secnumdepth}{5}
\setcounter{tocdepth}{5}


%Syntax for @startsection{name}{level}{indent}{abovespace}{belowspace}{format}
%The 4th argument is negative to suppress indenting after the section heading
\def\section{\@startsection{section}{2}{0pt}{-0.1pt}{0.1pt}{\singlespacing\hyphenpenalty=10000\centering\normalsize\bfseries}}%
\def\subsection{\@startsection{subsection}{2}{0pt}{-0.1pt}{0.1pt}{\singlespacing\hyphenpenalty=10000\normalsize\bfseries}}%
\def\subsubsection{\@startsection{subsubsection}{3}{0pt}{-0.1pt}{0.1pt}{\singlespacing\hyphenpenalty=10000\bfseries}}%
\def\paragraph{\@startsection{paragraph}{4}{0pt}{-0.1pt}{0.1pt}{\singlespacing\hyphenpenalty=10000\bfseries}}%
\def\subparagraph{\@startsection{subparagraph}{5}{0pt}{-0.1pt}{0.1pt}{\singlespacing\hyphenpenalty=10000\bfseries}}%

\def\presection{}
\def\presubsection{\textsc{Issue}\space}
\def\presubsubsection{}
\def\preparagraph{}
\def\presubparagraph{}
\def\postsection{}
\def\postsubsection{:}
\def\postsubsubsection{}
\def\postparagraph{}
\def\postsubparagraph{}

\newtoggle{needtotoc}
\toggletrue{needtotoc}

\newcommand\issue[1]{\subsection{#1}}
\WithSuffix\newcommand\issue*[1]{\togglefalse{needtotoc}\subsection{#1}\toggletrue{needtotoc}}

\newcounter{issues}
\setcounter{issues}{0}
\newcommand\NewIssue[2]{\csdef{#1@issue}{#2}\stepcounter{issues}}
\newcommand\PrintIssue[1]{\issue{\csuse{#1@issue}}}
\WithSuffix\newcommand\PrintIssue*[1]{\issue*{\csuse{#1@issue}}}


\def\@seccntformat#1{\csname pre#1\endcsname\csname the#1\endcsname\csname post#1\endcsname\quad}

%\def\numberline#1{\hb@xt@\@tempdima{#1\hfil}}

\def\numberline@tmp#1{\texorpdfstring{\hb@xt@\@tempdima{#1\hfil}}{{#1\space}}}

%\newcommand*\l@section{\@dottedtocline{1}{1.5em}{2.5em}}
%\newcommand*\l@subsection{\@dottedtocline{2}{3.8em}{3.2em}}
%\newcommand*\l@subsubsection{\@dottedtocline{3}{7.0em}{4.1em}}
%\newcommand*\l@paragraph{\@dottedtocline{4}{10em}{5em}}
%\newcommand*\l@subparagraph{\@dottedtocline{5}{12em}{6em}}



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
    \iftoggle{needtotoc}{\addcontentsline{toc}{#1}{%
      \ifnum #2>\c@secnumdepth \else
        \protect\numberline@tmp{\csname pre#1\endcsname\csname the#1\endcsname\csname post#1\endcsname\space}%
      \fi
      #8}}{}%
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




%No numbers printed for section (remove \quad); I. Subsection; A. Subsubsection
\renewcommand{\thesection}{\hspace{-1em}}
%\renewcommand{\thesubsection}{\arabic{subsection}}%\arabic{subsection}
\renewcommand{\thesubsection}{\ifnum\c@issues>1 \arabic{subsection}\else\fi}%\arabic{subsection}
\renewcommand\thesubsubsection{\ifnum\c@issues>1 \thesubsection.\else\fi\@arabic\c@subsubsection}
\renewcommand\theparagraph    {\thesubsubsection.\@arabic\c@paragraph}
\renewcommand\thesubparagraph {\theparagraph.\@arabic\c@subparagraph}

%==============================================================================
%%
%% \subsection{Configuring the Index / Table of Authorities}
%%
%==============================================================================

%%
%%_\indexindentsize_ --- Hanging indentation of toa from the left side (default: \the\indexindentsize)
%%
\newlength{\indexindentsize}
\setlength{\indexindentsize}{18pt}

%%
%%\noindent _\indexrightmargin_ --- Right margin for all-but-the-last lines of toa (default: \the\indexrightmargin)
%% The current value will preserve .5 inch of space for non-final lines, and ragged margins. To change the spacing on
%% the last line between the entry and the page numbers, you must edit the style file.
\newlength{\indexrightmargin}
\setlength{\indexrightmargin}{.5in plus 1.fil}

%%
%%\noindent _\passimlimit_ --- Use ``passim'' above this many pages (default: \passimlimit).
%% Can be changed with \\ _\renewcommand{\passimlimit}{5}_, etc.
%% Make sure to set this before the index is created (i.e. in the header).
%%
\newcommand{\passimlimit}{5}

%\pagenumber is the command to format page numbers in index rows
%  Each invocation adds the number to @assembledpagenums; incr. @passimcount
%  \@printpagenums called at end of each row, dumps values
\def\@pagenums{}
\newcounter{@passimcount}

\newcommand{\pagenumber}[1]{%
	\addtocounter{@passimcount}{1}%
	\ifnum\value{@passimcount}=1%
		\edef\@pagenums{#1}%
	\else%
		\ifnum\value{@passimcount}<\passimlimit%
			\edef\@pagenums{\@pagenums#1}%
		\else
			\def\@pagenums{\emph{passim}}%
		\fi%
	\fi%
	\IfSubStr{#1}{passim}{\setcounter{@passimcount}{\passimlimit}\def\@pagenums{\emph{passim}}}{}%
}

\newcommand{\@printpagenums}{%
	\@pagenums%
	\def\@pagenums{}%
	\setcounter{@passimcount}{0}%
}

%%
%%\noindent _\idxpassim_ --- Used in an index command, forces passim to always be used for a particular cite
%%
\newcommand{\idxpassim}[1]{passim}

\renewenvironment{theindex}
               {\parindent=\indexindentsize%
                \parskip=10pt plus .3pt%
				\linepenalty=1000%
				\hyphenpenalty=10000%
				\rightskip=\indexrightmargin%
				\parfillskip=-\indexrightmargin%
				\setcounter{@passimcount}{0}%
                \let\item\@idxitem
                \let\subitem\@idxsubitem}
               {\@printpagenums\par}

\renewcommand\@idxitem{\@printpagenums \par \hangindent 2\indexindentsize}
\providecommand\@idxsubitem{\@idxitem \hspace*{20pt}}

%\renewcommand\indexspace{\par \vskip 10pt plus 5pt minus 3pt \relax}

%%\noindent
%%_\caseindextitle_ --- Set the title for the case section of the table of authorities (Default: \@caseindextitle).
%%
%%\noindent
%%_\statutesindextitle_ --- Set the title for the statutes section of the table of authorities (Default: \@statutesindextitle).
%%
%%\noindent
%%_\otherindextitle_ --- Set the title for the miscellaneous section of the table of authorities (Default: \@otherindextitle).
%%
\def\@caseindextitle{Cases}
\newcommand{\caseindextitle}[1]{\def\@caseindextitle{#1}}

\def\@statutesindextitle{Constitutional Provisions, Statutes and Rules}
\newcommand{\statutesindextitle}[1]{\def\@statutesindextitle{#1}}

\def\@otherindextitle{Other Authorities}
\newcommand{\otherindextitle}[1]{\def\@otherindextitle{#1}}

%Generates files for each type of index entry
\makeindex{Case}
\makeindex{Statute}
\makeindex{Other}

\newtoggle{clearpage}
\toggletrue{clearpage}


%%
%% \noindent _\tableofauthorities_ --- Print the table of authorities.
%%

\newtoggle{printtoa}
\togglefalse{printtoa}
\newcommand*\checktoa{%
\global\togglefalse{printtoa}
\iffileexists{Case.ind}{\ifnum\pdf@filesize{Case.ind}>0\global\toggletrue{printtoa}\fi}{}
\iffileexists{Statute.ind}{\ifnum\pdf@filesize{Case.ind}>0\global\toggletrue{printtoa}\fi}{}
\iffileexists{Other.ind}{\ifnum\pdf@filesize{Case.ind}>0\global\toggletrue{printtoa}\fi}{}
}

\newcommand{\tableofauthorities}{%
\iftoggle{clearpage}{\clearpage}{}
\checktoa%
\iftoggle{printtoa}{%
\singlespacing
\section*{Table of Authorities}\addcontentsline{toc}{section}{Table of Authorities} %\pagestyle{toa}\thispagestyle{romanparen}%
	\iffileexists{Case.ind}{%
      \ifnum\pdf@filesize{Case.ind}>0
    \noindent\sc \@caseindextitle\hfill\rm Page\par
	\vspace{6pt}
	\everypar={\mark{\@caseindextitle---Continued}}
	\printindex{Case}\fi}{}
%
	\iffileexists{Statute.ind}{%
      \ifnum\pdf@filesize{Statute.ind}>0
		\everypar={\mark{}}
		~\par\noindent\sc \@statutesindextitle\hfill\rm\par\penalty10000
		\everypar={\mark{\@statutesindextitle---Continued}}
		\printindex{Statute}
		~\par
	\fi}{}
%
	\iffileexists{Other.ind}{%
      \ifnum\pdf@filesize{Other.ind}>0
		\everypar={\mark{}}
		\noindent\sc \@otherindextitle\rm\par\penalty10000
		\everypar={\mark{\@otherindextitle---Continued}}
		\printindex{Other}
	\fi}{}
	%\newpage
}{}}

\WithSuffix\newcommand\tableofauthorities*{%
\togglefalse{clearpage}\tableofauthorities\toggletrue{clearpage}}

%%
%%_\tableofcontents_ --- Print the table of contents.
%%
\renewcommand\tableofcontents{%
\iftoggle{clearpage}{\clearpage}{}
\section*{Table of Contents}
{\setlength{\parskip}{2ex plus 1ex minus .5ex}\@starttoc{toc}}}

\WithSuffix\newcommand\tableofcontents*{\togglefalse{clearpage}\tableofcontents\toggletrue{clearpage}}



\def\@snumwidth{2em}		%Size of box around toc section number
\def\@pnumwidth{2em}		%Size of right margin in toc for all but last line
\def\@smallpnumwidth{1em}	%Size of box put around toc pagenumber

%%
%%\noindent _\@tocline_ --- Sets the leaders to print aligned dots. Redefine to change.
%%
\newcommand{\@tocline}[2]{#1\nobreak \leaders \hbox to 0.125in{\hfil.\hfil} \hfill \nobreak\hb@xt@\@smallpnumwidth{\hss #2}\par}

\renewcommand{\@tocline}[2]{#1\nobreak \leaders \hbox to 0.125in{\hfil.\hfil} \hfill \nobreak\hb@xt@\@smallpnumwidth{\hss #2}\par}

%%
%%\noindent _\l@section_ --- Formats Top-level headings in the ToC flush left and small caps
%%
\renewcommand*\l@section[2]{%
    \addpenalty\@secpenalty%		Prefer to break before or after the tocline, not during
    \setlength\@tempdima{0em}%		Size of the space reserved for section number (which isn't there, so 0)
	{
      \parindent=0pt%				Dont automatically indent
	  \@tocline{{#1}}{#2}
	}
}


%%
%%\noindent _\l@subsection_ --- Formats Second-level headings indented from left with a hanging indent of _\@pnumwidth_
%%
\renewcommand*\l@subsection[2]{%
    \addpenalty\@secpenalty%		Prefer to break before or after the tocline, not during
    \@tempdima=5em%         Size of the space reserved for the subsection number
	{
      \parindent=0.0in%				Don't indent number; secname implicitly indented to \@tempdima
	  \rightskip=\@pnumwidth
      \parfillskip=-\@pnumwidth
      \leavevmode
      \advance\leftskip\@tempdima
      %\advance\leftskip by 0.25in
      \hskip -\leftskip
	  \@tocline{#1}{#2}
	}
}

%%
%%\noindent _\l@subsubsection_ --- Formats Third-level headings indented more from left with a hanging indent of _\@pnumwidth_
%%
\renewcommand*\l@subsubsection[2]{%
    \addpenalty\@secpenalty%		Prefer to break before or after the tocline, not during
    \@tempdima=\@snumwidth%         Size of the space reserved for the subsubsection letter
	{
      \parindent=\@snumwidth%		Indent first line equal to the previous subsection's letter indent
	  \rightskip=\@pnumwidth%		Reserve a larger-than-usual right margin
      \parfillskip=-\@pnumwidth%	And then compensate the last line, so that it is full width
      \leavevmode
      \advance\leftskip2\@tempdima% This is an odd trick that basically does the same as a \hangindent,
      \hskip -\leftskip%			but has the effect of keeping the leaders aligned
	  \@tocline{#1}{#2}
	}
}

\renewcommand*\l@paragraph[2]{%
    \addpenalty\@secpenalty%		Prefer to break before or after the tocline, not during
    \@tempdima=\@snumwidth%         Size of the space reserved for the subsubsection letter
	{
      \parindent=\@snumwidth%		Indent first line equal to the previous subsection's letter indent
	  \rightskip=\@pnumwidth%		Reserve a larger-than-usual right margin
      \parfillskip=-\@pnumwidth%	And then compensate the last line, so that it is full width
      \leavevmode
      \advance\leftskip2\@tempdima% This is an odd trick that basically does the same as a \hangindent,
      \hskip -\leftskip%			but has the effect of keeping the leaders aligned
	  \@tocline{#1}{#2}
	}
}

\renewcommand*\l@subsubsection{\@dottedtocline{3}{0.25in}{2em}}
\renewcommand*\l@paragraph{\@dottedtocline{4}{0.5in}{3em}}
\renewcommand*\l@subparagraph{\@dottedtocline{5}{0.75in}{4em}}

%==============================================================================
%%
%% \subsection{Page numbering commands}
%%
%% These commands are used by calling _\pagestyle{romanparen}_, _\thispagestyle{toa}_, etc.
%%
%==============================================================================
%%
%% \noindent Pagestyle _arabicparen_ --- arabic numerals inside parenthesis at bottom
%%
\newcommand{\ps@arabicparen}{%
	\def\thepage{\arabic{page}}
	\def\@oddhead{}%
	\def\@evenhead{}%
	\def\@oddfoot{\hfill(\arabic{page})\hfill}%
	\def\@evenfoot{\hfill(\arabic{page})\hfill}%
}

%%
%% \noindent Pagestyle _romanparen_ --- roman numerals inside parenthesis at bottom
%%
\newcommand{\ps@romanparen}{%
	\def\thepage{\roman{page}}
	\def\@oddhead{}%
	\def\@evenhead{}%
	\def\@oddfoot{\hfill(\roman{page})\hfill}%
	\def\@evenfoot{\hfill(\roman{page})\hfill}%
}

%%
%% \noindent Pagestyle _toa_ --- ``inherits'' from romanparen, but also puts the current mark at the top
%%
\newcommand{\ps@toa}{%
	\ps@romanparen%
	\def\@oddhead{\ifthenelse{\equal{\topmark}{\firstmark}}{\lower1em\hbox to \textwidth{{\sc\topmark} \hfill Page}}{\lower3em\hbox to \textwidth{\hfill Page}}}%
	\def\@evenhead{\ifthenelse{\equal{\topmark}{\firstmark}}{{\sc\topmark} \hfill Page}{\lower5em{\hfill Page}}}%
}

%%
%% \noindent Pagestyle _footertext_ --- Sets the argument to _\footertext_ in the footer, flush left
%%
\newcommand{\footertext}[1]{\def\@footertext{#1}}

\newcommand{\ps@footertext}{%
	\def\thepage{\roman{page}}
	\def\@oddhead{}%
	\def\@evenhead{}%
	\def\@oddfoot{\@footertext\hfill\def\@footertext{}}%
	\def\@evenfoot{\@footertext\hfill}%
}


%\newcommand{\sectionline}{\vbox{\baselineskip=1.5pt \hbox{\rule{\linewidth}{.7pt}}\hbox{\rule{\linewidth}{.7pt}}}}
\newcommand{\smallrule}{\parbox[c]{.3\linewidth}{\rule{\linewidth}{.7pt}}}

\newcommand\OralArgumentRequestedText{Oral Argument Requested}
\newtoggle{OralArgument}
\togglefalse{OralArgument}
\newcommand\OralArgumentRequested{\toggletrue{OralArgument}}

\renewenvironment{quotation}
               {\vskip0.5\baselineskip\begin{spacing}{1}\list{}{\listparindent 0em%
                        \itemindent\listparindent%
                        \rightmargin=1in%
                        \leftmargin=1in%
                        \topsep=0pt%
                        \itemsep=0pt%
                        \partopsep=0pt%
                        \parsep0.5\baselineskip
                        \parindent=0pt}%\z@ \@plus\p@
                \item\relax}
               {\end{spacing}\endlist}

%\newcommand\newquote[1]{\par\hskip1in\parbox{\linewidth-2in}{#1}}{}

             \def\notarize{%
               Sworn and subscribed before me on \today

\MakeShortSignature*{Notary}
}


\def\tableinterestedpersons{
\iftoggle{clearpage}{\clearpage}{}
\section{Certificate of Interested Persons}

\begin{doublespace}

The undersigned counsel of record in this case, \PrintPersonField{\@firstparty}{name} v. \PrintPersonField{\@secondparty}{name}, case number \csuse{aplstage@casenumber} certifies that the following listed persons and entities as described in the fourth sentence of Rule 28.2.1 have an interest in the outcome of this case. These representations are made in order that the judges of this court may evaluate possible disqualification or recusal.

\PrintClientName, currently incarcerated by the United States Bureau of Prisons.

\end{doublespace}

\MakeShortSignature{FB}
}



\def\statementoralargument{
\section{Statement Regarding Oral Argument}

\begin{doublespace}

\PrintClientName waives oral argument.

\end{doublespace}

}


\def\verification#1#2#3#4#5{%
  \begin{center}\textbf{Verification}\end{center}
  
My name is #1, my date of birth is #2, my address is #3. I declare  under  penalty of perjury that the foregoing is true and correct.

Executed in #4 County, #5 on \today.
}


\def\certificateofservice{%
\begin{center}\textbf{Certificate of Service}\end{center}

I served this document on \PrintPersonField{\@trialprosecutor}{name} of the \PrintPersonField{\@trialprosecutor}{office} by electronic service to \PrintPersonField{\@trialprosecutor}{email} on \today.
}

\def\certificateofserviceother#1{%
\begin{center}\textbf{Certificate of Service}\end{center}

On \today, I served this document on \PrintPersonField{\@trialprosecutor}{name} of the \PrintPersonField{\@trialprosecutor}{office} by #1 to:


\begin{center}\singlespace\noindent\PrintPersonField{\@trialprosecutor}{office}\\\PrintPersonField{\@trialprosecutor}{address}%\\PrintPersonField{\@trialprosecutor}{fax}
\end{center}
}

\def\certificateofcompliance#1{%
\begin{center}\textbf{Certificate of Compliance}\end{center}
This document contains #1 words.
}


\def\expunctionpersonalinfo{%
\section{Personal Information}
The petitioner is known by the following information:
\begin{itemize}
\item Full Name: \PrintPersonField{\@client}{name}
  
\item Sex: \PrintPersonField{\@client}{sex}

\item Race: \PrintPersonField{\@client}{race}

\item Date of birth: \PrintPersonField{\@client}{dateofbirth}

\item Texas driver's license number: \PrintPersonField{\@client}{DLN}

\item Social Security number: \PrintPersonField{\@client}{SSN}

\item Address at the time of the arrest: \PrintPersonField{\@client}{address}
\end{itemize}

}

\def\expunctionarrestinfo{%
\section{Arrest Information}
Charged offense: \csuse{trialstage@offensecharged}

Alleged offense date: \csuse{trialstage@dateofoffense}

Arrest date: \csuse{trialstage@dateofarrest}

City of arrest: \csuse{trialstage@cityofarrest}

County of arrest: \csuse{trialstage@countyofarrest}

Arresting agency: \csuse{trialstage@arrestingagency}

Case number: \csuse{trialstage@casenumber}

Court: \PrintCourtName{\csuse{trialstage@courttag}} %\PrintCourtPlace{\csuse{trialstage@courttag}}

  }

\def\sensitivedata{
  \begin{center}\textsc{This Document Contains Sensitive Data}\end{center}
  }


\def\printattorneyinfo{%
\iftitle@attorneyinfo
{\normalsize\noindent\parbox[t]{0.43\linewidth}{\raggedright\scshape\PrintFullPersonInfo{\secondary@lawyer}}%
\hfill\parbox[t]{0.43\linewidth}{\raggedright\scshape\PrintFullPersonInfo{\lead@lawyer}}}
\else\fi}

\def\printappealinfo{{\parskip=5pt%
\iftitle@vappeal
\PrintPersonField{\@firstparty}{name}\par
\hfill {\itshape\PrintPersonField{\@firstparty}{position}}\par
\textit{v.}\par\vskip10pt
\PrintPersonField{\@secondparty}{name}\par
\hfill{\itshape\PrintPersonField{\@secondparty}{position}}\par
\else
\textit{Ex parte}\quad\PrintPersonField{\@firstparty}{name}\par
\fi}}

\def\printappealfrom{%
\iftitle@appealfrom
\smallrule\vfilnew
On Appeal from the \PrintCourtName{\csuse{trialstage@courttag}}\\
\PrintCourtPlace{\csuse{trialstage@courttag}}\\
Case Number \csuse{trialstage@casenumber}{}\else\fi%
}

\def\vfilnew{\iftoggle{clearpage}{\vfil}{\par\vspace*{3mm}}}



\def\current@stage{\appellatestage@tag}
\def\@leadlawyerposition{Counsel for Appellant}

\def\makedoctitle{%
\edef\curr@court{\csuse{\current@stage @courttag}}%
\iftoggle{clearpage}{\clearpage}{}
\setlength{\parskip}{2ex plus 1ex minus .5ex}
\thispagestyle{empty}
{\centering\bfseries\csuse{aplstage@casenumber}\par}
{\scshape\large\centering
{\bfseries In the \PrintCourtName{\csuse{aplstage@courttag}}%
\edef\tmp@str{\csuse{\curr@court @place}}%
\ifx\tmp@str\@empty\else{ at~\PrintCourtPlace{\csuse{aplstage@courttag}}}\fi}\\[10mm]
\vfilnew
\printappealinfo
\vfilnew
\printappealfrom%\vskip-3mm
\vfilnew
\smallrule\par
\@title\par%\vskip-3mm
\smallrule
\vfilnew
\printattorneyinfo
\vfilnew
\iftitle@oralargument
{\centering\scshape\OralArgumentRequestedText\par}
\else\fi}
}

\WithSuffix\newcommand\makedoctitle*{%
\togglefalse{clearpage}\makedoctitle
% Commented out by fgb to fix page breaks for motions
%\toggletrue{clearpage}
}

%%
%% \noindent _\makefrontmatter_
%%
%% Creates all the boilerplate front matter, including the title page (by calling _\maketitle_),
%% the question presented (taken from the argument of _\questionpresented_), the table of contents and
%% the table of authorities (by calling _\tableofcontents_ and _\tableofauthorities_, respectively).
%%
\newcommand{\makefrontmatter}{%
\makedoctitle
\tableofindentityofparties
\tableofcontents
\tableofauthorities
}

\WithSuffix\newcommand\makefrontmatter*{%
\togglefalse{clearpage}\makefrontmatter\toggletrue{clearpage}}

\def\mainmatter{\iftoggle{clearpage}{\clearpage}{}\doublespacing\parskip=0pt}

\WithSuffix\newcommand\mainmatter*{%
\togglefalse{clearpage}\mainmatter\toggletrue{clearpage}}

%\newlength\tmpskip
%\setlength\tmpskip{0.3in}

\def\print@ipcitem#1{%
\noindent\parbox[t]{0.35\textwidth}{{\raggedright\PrintPersonField{#1}{position}\par}}
\hfill
\parbox[t]{0.6\textwidth}{\raggedright\PrintPersonNameWithAddress{#1}}
\vspace{1cm}}

\def\tableofindentityofparties{%
\iftoggle{clearpage}{\clearpage}{}
\section*{Identity of Parties and Counsel}

\print@ipcitem{\@firstparty}

\print@ipcitem{\@trialdefencecounsel}

\print@ipcitem{\@trialprosecutor}

\print@ipcitem{\@judgepresiding}

\print@ipcitem{\lead@lawyer}

\print@ipcitem{\@appealprosecutor}

}

\WithSuffix\newcommand\tableofindentityofparties*{%
\togglefalse{clearpage}\tableofindentityofparties\toggletrue{clearpage}}

\newcommand\crecord[2][]{\ifstrempty{#1}{\footnote{(C.R.~at~#2)}}{\footnote{(#1~C.R.~at~#2)}}%
}

\newcommand\rrecord[2][]{\ifstrempty{#1}{\footnote{(R.R.~at~#2)}}{\footnote{(#1~R.R.~at~#2)}}%
}

\newcommand\makesignature{%
\vskip\baselineskip
\begin{spacing}{1}
%\label{signatureblock}
\noindent\hspace{0.3\textwidth}\parbox[t]{0.7\textwidth}{\raggedright\large
/s/\space{\@leadlawyer}\enskip\\[-15pt]
\rule{10cm}{.25pt} \\
{\scshape\@leadlawyer}\\{\itshape\@leadlawyerposition}%\@address
}
\end{spacing}}

\newsavebox{\signature@box}

\def\print@signature#1{%
\savebox{\signature@box}{\raggedright\large
/s/\space\PrintPersonField{#1}{name}}
\noindent\usebox{\signature@box}\\[-10pt]
\rule{\wd\signature@box+1cm}{.25pt}
}

\def\print@esignature#1{%
\savebox{\signature@box}{\raggedright\large
\phantom{/s/\space\PrintPersonField{#1}{name}}}
\noindent\usebox{\signature@box}\\[-10pt]
\rule{\wd\signature@box+1cm}{.25pt}
}


\newcommand*\MakeSignature[1]{%
\hfill\parbox[t]{0.43\linewidth}{\begin{spacing}{1}\vskip\baselineskip
\print@signature{#1}\par
{\scshape\PrintPersonField{#1}{name}}\par
{\itshape\PrintPersonField{#1}{position}}\par
{\PrintPersonField{#1}{barnumber}}\par
{\PrintPersonField{#1}{address}}\par
{\PrintPersonField{#1}{phone}}\par
{\PrintPersonField{#1}{email}}
\vskip\baselineskip
\end{spacing}}}

\WithSuffix\newcommand\MakeSignature*[1]{%
\hfill\parbox[t]{0.43\linewidth}{\begin{spacing}{1}\vskip\baselineskip
\print@esignature{#1}\par
{\scshape\PrintPersonField{#1}{name}}\par
{\itshape\PrintPersonField{#1}{position}}\par
{\PrintPersonField{#1}{barnumber}}\par
{\PrintPersonField{#1}{address}}\par
{\PrintPersonField{#1}{phone}}\par
{\PrintPersonField{#1}{email}}
\vskip\baselineskip
\end{spacing}}}


\newcommand\makesignatureline[1]{%
\vskip\baselineskip
\begin{spacing}{1}
\noindent\hspace{0.3\textwidth}\parbox[t]{0.7\textwidth}{\raggedright\large
\mbox{} \\
/s/\space{#1}\enskip\\[-15pt]
\rule{10cm}{.25pt}\\
{\scshape #1}}
\end{spacing}}

\newcommand*\MakeShortSignature[1]{%
\hfill\parbox[t]{0.43\linewidth}{\begin{spacing}{1}\vskip\baselineskip
\print@signature{#1}\par
{\scshape\PrintPersonField{#1}{name}}\par
\vskip\baselineskip
\end{spacing}}}

\WithSuffix\newcommand\MakeShortSignature*[1]{%
\hfill\parbox[t]{0.43\linewidth}{\begin{spacing}{1}\vskip\baselineskip
\print@esignature{#1}\par
{\scshape\PrintPersonField{#1}{name}}\par
\vskip\baselineskip
\end{spacing}}}

%%
%% \noindent _\rightbox_ environment
%%
%% The contents of this environment will themselves be left-aligned, but the entire box
%% containing them will be as far to the right as possible. This is useful for signature-type
%% boxes at the end of the brief.
%%
\newenvironment{rightbox}{
	%Start a new group to contain the def of the testbox and newline
	\bgroup
	%Store the box here
	\newbox\testbox
	%Change newline so that each line is an hbox
	\def\\{\unskip\egroup\hbox\bgroup\ignorespaces}
	%Start vbox{hbox{ ...
	\setbox\testbox=\vbox\bgroup\hbox\bgroup\ignorespaces}
	%End the last hbox and the vbox
	{\unskip\egroup\egroup
	%Output a fill, then the box
	\hskip 0pt plus 1.0fill \box\testbox \egroup }

%==============================================================================
% Misc Default Settings
%==============================================================================

%Set the first pagestyle to have no number
%\thispagestyle{empty}

\clubpenalty=3000
\widowpenalty=3000
\parskip=0pt
\parsep=0pt
\parindent=2.5em



\SetupKeyvalOptions{
family = title,
prefix = title@
}

% Title page options for Appeal stage

\DeclareBoolOption{oralargument}
\DeclareComplementaryOption{hideoralargument}{oralargument}
\DeclareBoolOption{appealfrom}
\DeclareComplementaryOption{hideappealfrom}{appealfrom}
\DeclareBoolOption{attorneyinfo}
\DeclareComplementaryOption{hideattorneyinfo}{attorneyinfo}
\DeclareBoolOption{vappeal}
\DeclareComplementaryOption{eappeal}{vappeal}

% Title page options for Trial stage
\DeclareBoolOption{trialfiling}
\DeclareComplementaryOption{federalfiling}{trialfiling}
% \setkeys{title}{trialfiling=true}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Specific Documents

% Appellate

\if@appellate

\def\@title{}
\renewcommand\title[2][]{%
\setkeys{title}{oralargument=true,appealfrom=true,attorneyinfo=true,vappeal=true,#1}
\def\@title{#2}
}

\else\fi

% Trial

\if@trial

\def\@title{}
\renewcommand\title[2][]{%
\setkeys{title}{trialfiling=true,vappeal=true,#1}
\def\@title{#2}
}

\def\current@stage{\trialstage@tag}
\edef\curr@court{\csuse{\current@stage @courttag}}
\def\@leadlawyerposition{Counsel for Defendant}

\RequirePackage{enumitem}
\setlist[itemize,1]{label=---}
\setlist[itemize,2]{label=$\circ$}

\setlength\parindent{0.5in}
\setlength\parskip{3mm}

\def\section{\@startsection{section}{2}{0pt}{-10pt}{10pt}{\normalsize\bfseries}}%
\def\subsection{\@startsection{subsection}{2}{0pt}{-2pt}{2pt}{\hyphenpenalty=10000\normalsize\bfseries}}%
\def\subsubsection{\@startsection{subsubsection}{3}{0pt}{-2pt}{2pt}{\hyphenpenalty=10000\bfseries}}%
\def\paragraph{\@startsection{paragraph}{4}{0pt}{-2pt}{2pt}{\hyphenpenalty=10000\bfseries}}%
\def\subparagraph{\@startsection{subparagraph}{5}{0pt}{-2pt}{2pt}{\hyphenpenalty=10000\bfseries}}%

\def\presubsection{}
\def\postsection{.}
\def\postsubsection{}
%\def\postsubsubsection{}
%\def\postparagraph{}
%\def\postsubparagraph{}


\renewcommand{\thesection}{\arabic{section}}
\renewcommand{\thesubsection}{\thesection.\arabic{subsection}}%\arabic{subsection}
\renewcommand\thesubsubsection{\thesubsection.\@arabic\c@subsubsection}

\def\@snumwidth{2em}

\renewcommand*\l@section{\@dottedtocline{1}{1.5em}{2.5em}}
\renewcommand*\l@subsection{\@dottedtocline{2}{3.8em}{3.2em}}
\renewcommand*\l@subsubsection{\@dottedtocline{3}{7.0em}{4.1em}}
\renewcommand*\l@paragraph{\@dottedtocline{4}{10em}{5em}}
\renewcommand*\l@subparagraph{\@dottedtocline{5}{12em}{6em}}

\def\maketitle{\make@title}

\def\make@title{%
\iftitle@trialfiling
\thispagestyle{empty}
{\begin{center}
\ifx\csuse{\current@stage @casenumber}\@empty\rule{0.25\textwidth}{0.7pt}\else\csuse{\current@stage @casenumber}\fi
\vskip3mm
\parbox[c]{0.4\textwidth}{\centering%
\begin{spacing}{1.2}%
  % The below was the old way, replaces by calling \printappealinfo
  % \PrintPersonField{\@secondparty}{name}%
  % \\v.\\%
  % \PrintPersonField{\@firstparty}{name}%
\printappealinfo
\end{spacing}}
\vrule
\parbox[c]{0.45\textwidth}{\centering%
\begin{spacing}{1.2}
\edef\curr@court{\csuse{\current@stage @courttag}}
\PrintCourtName{\curr@court}%
\edef\tmp@str{\csuse{\curr@court @place}}%
\ifx\tmp@str\@empty\else{\\\PrintCourtPlace{\curr@court}}\fi
\end{spacing}}
\par\vskip5mm
{\bfseries\@title}
\end{center}}
\else
{\begin{center}
\edef\curr@court{\csuse{\current@stage @courttag}}
\textsc{\PrintCourtName{\curr@court}%
\edef\tmp@str{\csuse{\curr@court @place}}%
\ifx\tmp@str\@empty\else{\\~\PrintCourtPlace{\curr@court}}\fi}\par
\vskip3mm
\parbox[t]{0.45\textwidth}{\centering%
\begin{spacing}{1.2}%
\PrintPersonField{\@secondparty}{name}%
\\v.\\%
\PrintPersonField{\@firstparty}{name}%
\end{spacing}}
\vrule%\parbox[t]{1cm}{\rule{0.5pt}{2cm}}%
\parbox[t]{0.4\textwidth}{\centering%
\begin{spacing}{1.2}
\ifx\@docket\@empty\rule{0.25\textwidth}{0.7pt}\else\csuse{\current@stage @casenumber}\fi\par
\csuse{\current@stage @extrainfo}
\end{spacing}}%
\par\vskip5mm
{\bfseries\@title}
\end{center}
}\fi}

\else\fi

% Expunction

\if@expunction

\def\@title{}
\renewcommand\title[2][]{%
\setkeys{title}{trialfiling=true,eappeal=true,#1}
\def\@title{#2}
}

\def\current@stage{\expunctionstage@tag}
\edef\curr@court{\csuse{\current@stage @courttag}}
\def\@leadlawyerposition{Counsel for Petitioner}

\RequirePackage{enumitem}
\setlist[itemize,1]{label=---}
\setlist[itemize,2]{label=$\circ$}

\setlength\parindent{0.5in}
\setlength\parskip{3mm}

\def\section{\@startsection{section}{2}{0pt}{-10pt}{10pt}{\normalsize\bfseries}}%
\def\subsection{\@startsection{subsection}{2}{0pt}{-2pt}{2pt}{\hyphenpenalty=10000\normalsize\bfseries}}%
\def\subsubsection{\@startsection{subsubsection}{3}{0pt}{-2pt}{2pt}{\hyphenpenalty=10000\bfseries}}%
\def\paragraph{\@startsection{paragraph}{4}{0pt}{-2pt}{2pt}{\hyphenpenalty=10000\bfseries}}%
\def\subparagraph{\@startsection{subparagraph}{5}{0pt}{-2pt}{2pt}{\hyphenpenalty=10000\bfseries}}%

\def\presubsection{}
\def\postsection{.}
\def\postsubsection{}
%\def\postsubsubsection{}
%\def\postparagraph{}
%\def\postsubparagraph{}


\renewcommand{\thesection}{\arabic{section}}
\renewcommand{\thesubsection}{\thesection.\arabic{subsection}}%\arabic{subsection}
\renewcommand\thesubsubsection{\thesubsection.\@arabic\c@subsubsection}

\def\@snumwidth{2em}

\renewcommand*\l@section{\@dottedtocline{1}{1.5em}{2.5em}}
\renewcommand*\l@subsection{\@dottedtocline{2}{3.8em}{3.2em}}
\renewcommand*\l@subsubsection{\@dottedtocline{3}{7.0em}{4.1em}}
\renewcommand*\l@paragraph{\@dottedtocline{4}{10em}{5em}}
\renewcommand*\l@subparagraph{\@dottedtocline{5}{12em}{6em}}

\def\maketitle{\make@title}

\def\make@title{%
\thispagestyle{empty}
{\begin{center}
\ifx\csuse{\current@stage @casenumber}\@empty\rule{0.25\textwidth}{0.7pt}\else\csuse{\current@stage @casenumber}\fi
\vskip3mm
\parbox[c]{0.4\textwidth}{\centering%
\begin{spacing}{1.2}%
\printappealinfo
\end{spacing}}
\vrule
\parbox[c]{0.45\textwidth}{\centering%
\begin{spacing}{1.2}
\edef\curr@court{\csuse{\current@stage @courttag}}
\PrintCourtName{\curr@court}%
\edef\tmp@str{\csuse{\curr@court @place}}%
\ifx\tmp@str\@empty\else{\\\PrintCourtPlace{\curr@court}}\fi
\end{spacing}}
\par\vskip5mm
{\bfseries\@title}
\end{center}}
}

\fi

%\else
%\fi




% vim: set syntax=tex :
