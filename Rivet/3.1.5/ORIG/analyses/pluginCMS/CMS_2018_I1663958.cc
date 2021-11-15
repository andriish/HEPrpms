#include "Rivet/Analysis.hh"
#include "Rivet/Projections/FinalState.hh"
#include "Rivet/Projections/VisibleFinalState.hh"
#include "Rivet/Projections/VetoedFinalState.hh"
#include "Rivet/Projections/FastJets.hh"
#include "Rivet/Projections/DressedLeptons.hh"
#include "Rivet/Tools/BinnedHistogram.hh"

namespace Rivet {


  /// @brief ttbar lepton+jets 13 TeV
  class CMS_2018_I1663958 : public Analysis {
  public:

    RIVET_DEFAULT_ANALYSIS_CTOR(CMS_2018_I1663958);

    void init() {

      const FinalState fs(Cuts::abseta < 6.);
      const VisibleFinalState vfs(Cuts::abseta < 6.);

      VetoedFinalState invisibles(fs);
      invisibles.addVetoOnThisFinalState(vfs);
      declare(invisibles, "Invisibles");

      FinalState all_photons(vfs, Cuts::abspid == PID::PHOTON);
      FinalState leptons(vfs, Cuts::abspid == PID::ELECTRON || Cuts::abspid == PID::MUON);

      DressedLeptons dressed_leptons(all_photons, leptons, 0.1, Cuts::abseta < 2.4 && Cuts::pT > 15*GeV, true);
      declare(dressed_leptons, "MyLeptons");

      VetoedFinalState photons(all_photons);
      photons.addVetoOnThisFinalState(dressed_leptons);
      declare(photons, "MyPhotons");

      VetoedFinalState isolationparticles(vfs);
      isolationparticles.addVetoOnThisFinalState(dressed_leptons);
      declare(isolationparticles, "IsoParticles");

      declare(FastJets(vfs, FastJets::ANTIKT, 0.4), "Jets");

      book(_h["thadpt"], 1, 1, 1);
      book(_h["thady"],  3, 1, 1);
      book(_h["tleppt"], 5, 1, 1);
      book(_h["tlepy"],  7, 1, 1);
      book(_h["ttm"],    9, 1, 1);
      book(_h["ttpt"],  11, 1, 1);
      book(_h["tty"],   13, 1, 1);
      book(_h["njet"],  15, 1, 1);
      /// @todo Memory leak
      vector<double> njetbins = {-0.5, 0.5, 1.5, 2.5, 3.5};
      for (size_t i = 0; i < njetbins.size() - 1; ++i) {
        { Histo1DPtr tmp; _b["njet_ttm"].add(njetbins[i], njetbins[i+1], book(tmp, 17 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["njet_ttm_norm"].add(njetbins[i], njetbins[i+1], book(tmp, 99 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["njet_thadpt"].add(njetbins[i], njetbins[i+1], book(tmp, 22 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["njet_thadpt_norm"].add(njetbins[i], njetbins[i+1], book(tmp, 104 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["njet_ttpt"].add(njetbins[i], njetbins[i+1], book(tmp, 27 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["njet_ttpt_norm"].add(njetbins[i], njetbins[i+1], book(tmp, 109 + i, 1, 1)); }
      }
      vector<double> thadybins = {0.0, 0.5, 1.0, 1.5, 2.5};
      for (size_t i = 0; i < thadybins.size() - 1; ++i) {
        { Histo1DPtr tmp; _b["thady_thadpt"].add(thadybins[i], thadybins[i+1], book(tmp, 32 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["thady_thadpt_norm"].add(thadybins[i], thadybins[i+1], book(tmp, 114 + i, 1, 1)); }
      }
      vector<double> ttmbins = {300., 450., 625., 850., 2000.};
      for (size_t i = 0; i < ttmbins.size() - 1; ++i) {
        { Histo1DPtr tmp; _b["ttm_tty"].add(ttmbins[i], ttmbins[i+1], book(tmp, 37 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["ttm_tty_norm"].add(ttmbins[i], ttmbins[i+1], book(tmp, 119 + i, 1, 1)); }
      }
      vector<double> thadptbins = {0., 90., 180., 270., 800.};
      for (size_t i = 0; i < thadptbins.size() - 1; ++i) {
        { Histo1DPtr tmp; _b["thadpt_ttm"].add(thadptbins[i], thadptbins[i+1], book(tmp, 42 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["thadpt_ttm_norm"].add(thadptbins[i], thadptbins[i+1], book(tmp, 124 + i, 1, 1)); }
      }
      vector<double> jetbins = {-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5};
      for (size_t i = 0; i < jetbins.size() - 1; ++i) {
        { Histo1DPtr tmp; _b["jetspt"].add(jetbins[i], jetbins[i+1], book(tmp, 47 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["jetspt_norm"].add(jetbins[i], jetbins[i+1], book(tmp, 129 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["jetseta"].add(jetbins[i], jetbins[i+1], book(tmp, 56 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["jetseta_norm"].add(jetbins[i], jetbins[i+1], book(tmp, 138 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["jetsdr"].add(jetbins[i], jetbins[i+1], book(tmp, 65 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["jetsdr_norm"].add(jetbins[i], jetbins[i+1], book(tmp, 147 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["jetsdrtops"].add(jetbins[i], jetbins[i+1], book(tmp, 74 + i, 1, 1)); }
        { Histo1DPtr tmp; _b["jetsdrtops_norm"].add(jetbins[i], jetbins[i+1], book(tmp, 156 + i, 1, 1)); }
      }
      vector<double> njetsptbins = {0., 40., 60., 80., 120.};
      for (size_t i = 0; i < njetsptbins.size() - 1; ++i) {
        { Histo1DPtr tmp; _b["njetspt"].add(njetsptbins[i], njetsptbins[i+1], book(tmp, 169 + i, 1, 1)); }
      }

      book(_h["thadpt_norm"], 83, 1, 1);
      book(_h["thady_norm"],  85, 1, 1);
      book(_h["tleppt_norm"], 87, 1, 1);
      book(_h["tlepy_norm"],  89, 1, 1);
      book(_h["ttm_norm"],    91, 1, 1);
      book(_h["ttpt_norm"],   93, 1, 1);
      book(_h["tty_norm"],    95, 1, 1);
      book(_h["njet_norm"],   97, 1, 1);
      /// @todo Memory leak
      book(m_hist_gap1, "d165-x01-y01");
      book(m_hist_gap2, "d167-x01-y01");
    }


    void analyze(const Event& event) {

      Jets bjets, ljets;
      Particles leptons, vetoleptons, additionalobjects, additionaljets;

      const Particles& isopars = apply<VetoedFinalState>(event, "IsoParticles").particles();
      const Particles& dressedleptons = apply<DressedLeptons>(event, "MyLeptons").particles();
      for (const Particle& lep : dressedleptons) {
        double isolation = sum(filter_select(isopars, deltaRLess(lep, 0.4)), Kin::pT, 0.);
        isolation = isolation/lep.pt();
        if (isolation > 0.35) continue;
        if (lep.pt() > 30*GeV && lep.abseta() < 2.4) leptons += lep;
        else if(lep.pt() > 15*GeV && lep.abseta() < 2.4)  vetoleptons += lep;
      }

      const Particles& photons = apply<VetoedFinalState>(event, "MyPhotons").particles(Cuts::abseta < 2.4 && Cuts::pT > 15*GeV);
      for (const Particle& ph : photons) {
        double isolation = sum(filter_select(isopars, deltaRLess(ph, 0.4)), pT, 0.);
        isolation = isolation/ph.pt() - 1.;
        if (isolation > 0.25) continue;
        additionalobjects += ph;
      }

      FourMomentum invsum(0.,0.,0.,0.);
      const Particles& invfspars = apply<FinalState>(event, "Invisibles").particles();
      for (const Particle& par : invfspars) invsum += par.mom();

      Jets allJets = apply<FastJets>(event, "Jets").jetsByPt(Cuts::abseta < 2.4 && Cuts::pT > 25*GeV);
      idiscardIfAnyDeltaRLess(allJets, leptons, 0.4);
      idiscardIfAnyDeltaRLess(allJets, vetoleptons, 0.4);
      idiscardIfAnyDeltaRLess(allJets, additionalobjects, 0.4);
      for (const Jet& jet : allJets) {
        if (jet.bTagged())  bjets += jet;
        else                ljets += jet;
      }

      //Semi-leptonic reconstruction
      if (leptons.size() != 1 || vetoleptons.size() || bjets.size() < 2 || ljets.size() < 2)  vetoEvent;

      FourMomentum wl = invsum + leptons[0].mom();

      Particle thad, tlep;
      Particles tlep_decay(3), thad_decay(3);
      double Kmin = numeric_limits<double>::max();
      for (size_t a = 0 ; a <  ljets.size() ; ++a){
        const Jet& lja = ljets[a];
        for (size_t b = 0 ; b < a ; ++b) {
          const Jet& ljb = ljets[b];
          FourMomentum wh(lja.momentum() + ljb.momentum());
          for (const Jet& bjh : bjets) {
            FourMomentum th(wh + bjh.momentum());
            for (const Jet& bjl : bjets) {
              if (&bjh == &bjl) continue;
              FourMomentum tl(wl + bjl.momentum());

              double K = pow(wh.mass() - 80.4, 2) + pow(th.mass() - 172.5, 2) + pow(tl.mass() - 172.5, 2);
              if (K < Kmin)
              {
                Kmin = K;
                thad = Particle(6, th);
                thad_decay[0] = Particle(5, bjh);
                thad_decay[1] = lja.pt() > ljb.pt() ? Particle(1, lja) : Particle(1, ljb);
                thad_decay[2] = lja.pt() <= ljb.pt() ? Particle(1, lja) : Particle(1, ljb);
                tlep = Particle(-6, tl);
                tlep_decay[0] = Particle(5, bjl);
                tlep_decay[1] = leptons[0];
                tlep_decay[2] = Particle(-1*(leptons[0].pid()+1), invsum);
              }
            }
          }
        }
      }

      Particles tt_jets({ tlep_decay[0], thad_decay[0], thad_decay[1], thad_decay[2] });

      const double eps = 1E-5;
      for (const Jet& jet : bjets) {
        if(jet.pt() < 30*GeV || jet.abseta() > 2.4) continue;
        if(find_if(tt_jets.begin(), tt_jets.end(), [&](const Particle& par){return deltaR(jet, par) < eps;}) != tt_jets.end()) continue;
        additionaljets += Particle(5, jet.mom());
      }
      for (const Jet& jet : ljets) {
        if(jet.pt() < 30*GeV || jet.abseta() > 2.4) continue;
        if(find_if(tt_jets.begin(), tt_jets.end(), [&](const Particle& par){return deltaR(jet, par) < eps;}) != tt_jets.end()) continue;
        if(jet.cTagged())  additionaljets += Particle(4, jet.mom());
        else               additionaljets += Particle(1, jet.mom());
      }

      sort(additionaljets.begin(), additionaljets.end(), cmpMomByPt);

      FourMomentum tt(thad.mom() + tlep.mom());

      dualfill("thadpt", thad.pt()/GeV);
      dualfill("thady", thad.absrap());
      dualfill("tleppt", tlep.pt()/GeV);
      dualfill("tlepy", tlep.absrap());
      dualfill("ttm", tt.mass()/GeV);
      dualfill("ttpt", tt.pt()/GeV);
      dualfill("tty", tt.absrap());
      dualfill("njet", min(additionaljets.size(), (size_t)5));
      double njet = double(min((size_t)3, additionaljets.size()));
      dualfill("njet_ttm", njet, tt.mass()/GeV);
      dualfill("njet_thadpt", njet, thad.pt()/GeV);
      dualfill("njet_ttpt", njet, tt.pt()/GeV);
      dualfill("thady_thadpt", thad.absrap(), thad.pt()/GeV);
      dualfill("ttm_tty", tt.mass()/GeV, tt.absrap());
      dualfill("thadpt_ttm", thad.pt()/GeV, tt.mass()/GeV);
      int jpos = -1;
      for (const Particles& jets : {tt_jets, additionaljets}) {
        for (const Particle& jet : jets) {
          jpos++;
          dualfill("jetspt", jpos, jet.pt()/GeV);
          dualfill("jetseta", jpos, jet.abseta());
          double drmin = 1E10;
          for (const Particle& tjet : tt_jets) {
            double dr = deltaR(jet, tjet);
            if(dr > eps && dr < drmin)  drmin = dr;
          }
          dualfill("jetsdr", jpos, drmin);
          dualfill("jetsdrtops", jpos, min(deltaR(jet, thad), deltaR(jet, tlep)));
        }
      }
      for (double ptcut : {30, 50, 75, 100}) {
        _b["njetspt"].fill(ptcut , count_if(additionaljets.begin(), additionaljets.end(),
                                            [&ptcut](const Particle& j) {return j.pt() > ptcut;}));
      }
    }

    void dualfill(const string& tag, const double value) {
      _h[tag]->fill(value);  _h[tag + "_norm"]->fill(value);
    }

    void dualfill(const string& tag, const double val1, const double val2) {
      _b[tag].fill(val1, val2);  _b[tag + "_norm"].fill(val1, val2);
    }

    void gapfractionfromjetpt(const string& tag, Scatter2DPtr hgap, size_t njet) {
      size_t hn = njet+3;
      const double total = _b[tag].histo(0)->integral();
      const double totalj = _b[tag].histo(hn)->integral();
      double acc = total-totalj;
      double gf = acc/total;
      if (!std::isnan(gf)) {
        for (size_t nb = 0 ; nb < _b[tag].histo(hn)->numBins() ; ++nb) {
          const double bl = _b[tag].histo(njet+3)->bin(nb).xMin();
          const double bh = _b[tag].histo(njet+3)->bin(nb).xMax();
          const double bc = 0.5*(bh+bl);
          hgap->addPoint(bc, gf, bc-bl, bh-bc, 0., 0.);
          acc += _b[tag].histo(njet+3)->bin(nb).area();
          gf = acc/total;
        }
      } else  MSG_WARNING("Gap fraction is NaN. Histogram not produced.");
    }


    void finalize() {

      const double sf = crossSection()/sumOfWeights();

      gapfractionfromjetpt("jetspt", m_hist_gap1, 1);
      gapfractionfromjetpt("jetspt", m_hist_gap2, 2);

      for (auto& item : _h) {
        if (item.first.find("_norm") != string::npos) {
          normalize(item.second, 1.0, false);
        }
        else  scale(item.second, sf);
      }

      for (auto& item : _b) {
        if (item.first.find("_norm") != string::npos) {
          double area = 0;
          for (auto& hist : item.second.histos()) {  area += hist->sumW(false); }
          //normalize(item.second.histos(), 1.0, false);
          if (area)  item.second.scale(1/area, this);
        }
        else if (item.first.find("njetspt") != string::npos) {
          // skip division by bin width for secondary axis
          for (auto& hist : item.second.histos()) { scale(hist, sf); }
        }
        else  item.second.scale(sf, this);
      }

    }

    map<string,Histo1DPtr> _h;
    map<string,BinnedHistogram> _b;

    Scatter2DPtr m_hist_gap1;
    Scatter2DPtr m_hist_gap2;

  };


  RIVET_DECLARE_PLUGIN(CMS_2018_I1663958);
}
